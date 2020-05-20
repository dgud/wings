%%
%%  wings_image_viewer.erl --
%%
%%     A simple image viewer
%%
%%  Copyright (c) 2014 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_image_viewer).
-export([new/2, new/3]).

%% Internal exports
-export([init/1, terminate/2, code_change/3,
	 handle_sync_event/3, handle_event/2,
	 handle_cast/2, handle_info/2, handle_call/3]).

-behaviour(wx_object).

-include("wings.hrl").
-include_lib("wings/e3d/e3d_image.hrl").

%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new(WinName, Image) -> new(WinName, Image, []).

new(WinName, E3d=#e3d_image{name=Name, type=Type}, Opts) ->
    new(WinName, wings_image:e3d_to_wxImage(E3d),
	[{name,Name}, {type,Type}, destroy_after|Opts]);
new(WinName, Filename, Opts) when is_list(Filename) ->
    BlockWxMsgs = wxLogNull:new(),
    Img = wxImage:new(Filename),
    true = wxImage:ok(Img), %% Assert
    wxLogNull:destroy(BlockWxMsgs),
    new(WinName, Img, [{name,Filename}, destroy_after|Opts]);
new(WinName, Image, Opts) ->
    wxImage = wx:getObjectType(Image), %% Assert
    Name = proplists:get_value(name, Opts, ""),
    Type = proplists:get_value(type, Opts, ""),
    H0 = wxImage:getHeight(Image),
    W0 = wxImage:getWidth(Image),
    Title = lists:flatten(io_lib:format(?__(1,"Image: ~ts [~wx~w] ~s"),[Name,W0,H0,Type])),
    Size = {size,{min(800,max(200,W0+100)), min(600,max(150,H0+100))}},
    {Frame,Ps} = wings_frame:make_win(Title, [Size]),
    Window = wings_sup:window(undefined, ?MODULE, [Frame, WinName, Image, Opts]),
    wings_wm:toplevel(WinName, Window, Ps, {push, fun(Ev) -> forward_ev(Ev, WinName, Window) end}),
    wxWindow:refresh(Window),
    keep.

forward_ev({note, image_change}, {_, WinId}, Window) ->
    case [Im || {Id, Im} <- wings_image:images(), WinId =:= Id] of
        [Image] -> wx_object:cast(Window, {image_change, Image});
        _ -> ignore
    end,
    keep;
forward_ev(_, _, _) ->
    keep.

%%%%%%%% Progress bar internals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {panel, ref, bitmap, bgb, scale=1.0, menu, origo={0,0}, prev}).

init([Frame, Ref, Image, Opts]) ->
    Panel = wxPanel:new(Frame, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),

    BM = wxBitmap:new(Image),
    proplists:get_value(destroy_after, Opts, false)
	andalso wxImage:destroy(Image),
    BGB = wxBrush:new({200,200,200}, [{style, ?wxCROSS_HATCH}]),

    wxPanel:connect(Panel, mousewheel),
    wxPanel:connect(Panel, right_up),
    wxPanel:connect(Panel, left_down),
    wxPanel:connect(Panel, motion),
    wxPanel:connect(Panel, erase_background), %% WIN32 only?
    wxPanel:connect(Panel, paint, [callback]),
    wxPanel:connect(Panel, enter_window, [{userData, {win, Panel}}]),
    wxFrame:show(Frame),
    %% wxFrame:createStatusBar(Frame),
    %% wxFrame:setStatusText(Frame, io_lib:format("Scale: ~w%", [100])),
    {Panel, #state{ref=Ref, panel=Panel, bitmap=BM, bgb=BGB}}.

-define(wxGC, wxGraphicsContext).

handle_sync_event(#wx{obj=Panel, event=#wxPaint{}}, _,
		  #state{bitmap=Image, bgb=BGB, scale=Scale, origo={X,Y}}) ->
    DC = case os:type() of
	     {win32, _} -> %% Flicker on windows
	     	 wx:typeCast(wxBufferedPaintDC:new(Panel), wxPaintDC);
	     _ ->
		 wxPaintDC:new(Panel)
	 end,
    {W0,H0} = wxPanel:getClientSize(Panel),
    wxDC:setBackground(DC, ?wxWHITE_BRUSH),
    wxDC:clear(DC),

    H = wxBitmap:getHeight(Image),
    W = wxBitmap:getWidth(Image),

    GC = ?wxGC:create(DC),
    ?wxGC:setBrush(GC, BGB),
    ?wxGC:drawRectangle(GC, 0, 0, W0, H0),
    ?wxGC:translate(GC, X+(W0-Scale*W) / 2,Y+(H0-Scale*H) / 2),
    ?wxGC:scale(GC, Scale, Scale),
    ?wxGC:drawBitmap(GC, Image, 0,0, W,H),
    wxPaintDC:destroy(DC),
    %% wxFrame:setStatusText(Frame, io_lib:format("Scale: ~w%", [round(Scale*100.0)])),
    ok.

handle_event(#wx{event=#wxMouse{type=mousewheel, wheelRotation=Rot}},
	     #state{scale=Scale0, panel=Panel}=S) ->
    Scale = if Rot > 0.0 -> Scale0*1.2;
	      true -> Scale0/1.2
	   end,
    wxWindow:refresh(Panel),
    {noreply, S#state{scale=Scale}};

handle_event(#wx{event = #wxMouse{type = right_up}}, State=#state{panel=Panel}) ->
    Menu = wxMenu:new([]),
    wxMenu:append(Menu, 1012, "12%"),
    wxMenu:append(Menu, 1025, "25%"),
    wxMenu:append(Menu, 1050, "50%"),
    wxMenu:appendSeparator(Menu),
    wxMenu:append(Menu, 1100, "100%"),
    wxMenu:appendSeparator(Menu),
    wxMenu:append(Menu, 1200, "200%"),
    wxMenu:append(Menu, 1400, "400%"),
    wxMenu:append(Menu, 1800, "800%"),
    wxMenu:connect(Menu, command_menu_selected),
    wxWindow:popupMenu(Panel, Menu),
    {noreply, State#state{menu=Menu}};

handle_event(#wx{id=MenuId, event = #wxCommand{}},
	     State = #state{menu=Menu, panel=Panel}) ->
    wxMenu:destroy(Menu),
    wxWindow:refresh(Panel),
    {noreply, State#state{scale=(MenuId-1000)/100}};

handle_event(#wx{event=#wxMouse{type=motion, leftDown=true, x=X, y=Y}},
	     State=#state{prev={XP,YP}, origo={Xo,Yo}, panel=Panel}) ->
    wxWindow:refresh(Panel),
    {noreply, State#state{origo={Xo+X-XP,Yo+Y-YP}, prev={X,Y}}};

handle_event(#wx{event=#wxMouse{type=motion}}, State=#state{}) ->
    {noreply, State};

handle_event(#wx{event=#wxMouse{type=left_down,x=X,y=Y}}, State) ->
    {noreply, State#state{prev={X,Y}}};

handle_event(#wx{event=#wxMouse{type=enter_window}}=Ev, #state{panel=Panel}=State) ->
    wxWindow:setFocus(Panel),
    wings_frame ! Ev,
    {noreply, State};

handle_event(#wx{event=#wxErase{}}, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, keep, State}.

handle_cast({image_change, E3d}, #state{panel=Panel,bitmap=Old}=State) ->
    wxBitmap:destroy(Old),
    Image = wings_image:e3d_to_wxImage(E3d),
    BM = wxBitmap:new(Image),
    wxImage:destroy(Image),
    wxWindow:refresh(Panel),
    {noreply, State#state{bitmap=BM}};
handle_cast(_, State) -> {noreply, State}.

handle_info(_, State) -> {noreply, State}.

code_change(_, _, State) -> State.

terminate(_Reason, #state{ref=Ref, bgb=BGB, bitmap=BM}) ->
    %%    io:format("terminate: ~p (~p)~n",[?MODULE, _Reason]),
    wings ! {wm, {delete, Ref}},
    wxBitmap:destroy(BM),
    wxBrush:destroy(BGB),
    ok.
