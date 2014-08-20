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
-export([new/1, new/2]).

%% Internal exports
-export([init/1, terminate/2, code_change/3,
	 handle_sync_event/3, handle_event/2,
	 handle_cast/2, handle_info/2, handle_call/3]).

-behaviour(wx_object).

-include("wings.hrl").
-include_lib("wings/e3d/e3d_image.hrl").

%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new(Image) -> new(Image, []).

new(E3d=#e3d_image{name=Name}, Opts) ->
    new(wings_image:e3d_to_wxImage(E3d),
	[{name,Name}, destroy_after|Opts]);
new(Filename, Opts) when is_list(Filename) ->
    new(wxImage:new(Filename), [{name,Filename}, destroy_after|Opts]);
new(Image, Opts) ->
    wxImage = wx:getObjectType(Image), %% Assert
    Parent = case get(top_frame) of
		 undefined -> wx:null();
		 TopFrame -> TopFrame
	     end,
    Frame = wx_object:start_link(?MODULE, [Parent, Image, Opts], []),
    wxFrame:show(Frame),
    Frame.


%%%%%%%% Progress bar internals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {frame, panel, bitmap, bgb, scale=1.0, menu, origo={0,0}, prev}).

init([Parent, Image, Opts]) ->
    Name = proplists:get_value(name, Opts, ""),
    H0 = wxImage:getHeight(Image),
    W0 = wxImage:getWidth(Image),
    Title = lists:flatten(io_lib:format(?__(1,"Image: ~s [~wx~w]"),[Name,W0,H0])),
    Frame = wxFrame:new(Parent, -1, Title,
			[{size,{min(800,max(200,W0+100)), min(600,max(150,H0+100))}}]),
    Panel = wxPanel:new(Frame, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),

    wxPanel:connect(Panel, paint, [callback]),
    wxPanel:connect(Panel, erase_background), %% WIN32 only?

    wxPanel:connect(Panel, mousewheel),
    wxPanel:connect(Panel, right_up),
    wxPanel:connect(Panel, left_down),
    wxPanel:connect(Panel, motion),

    BM = wxBitmap:new(Image),
    proplists:get_value(destroy_after, Opts, false)
	andalso wxImage:destroy(Image),

    wxFrame:createStatusBar(Frame),
    wxFrame:setStatusText(Frame, io_lib:format("Scale: ~w%", [100])),
    BGB = wxBrush:new({200,200,200}, [{style, ?wxCROSS_HATCH}]),
    {Frame, #state{frame=Frame, panel=Panel, bitmap=BM, bgb=BGB}}.

-define(wxGC, wxGraphicsContext).

handle_sync_event(#wx{obj=Panel, event=#wxPaint{}}, _,
		  #state{frame=Frame, bitmap=Image, bgb=BGB, scale=Scale, origo={X,Y}}) ->
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
    wxFrame:setStatusText(Frame, io_lib:format("Scale: ~w%", [round(Scale*100.0)])),
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

handle_event(#wx{event=#wxErase{}}, State) ->
    {noreply, State}.

handle_call(Req, _From, State) ->
    {reply, {error, Req}, State}.

handle_cast(_, State) -> State.

handle_info(_, State) -> State.

code_change(_, _, State) -> State.

terminate(_, #state{bgb=BGB, bitmap=BM}) ->
    wxBitmap:destroy(BM),
    wxBrush:destroy(BGB),
    ok.
