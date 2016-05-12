%%
%%  wings_frame.erl --
%%
%%     The top frame and window manager of Wings 3D.
%%
%%  Copyright (c) 2015 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wings_frame).

-export([top_menus/0, make_external_win/3, register_win/1, get_icon_images/0]).

-export([start/1, forward_event/1]).

%% Internal
-behaviour(wx_object).
-export([init/1,
	 handle_call/3, handle_cast/2, handle_event/2, handle_info/2,
	 code_change/3, terminate/2]).

-define(NEED_ESDL, 1). %% event mapping
-include("wings.hrl").

%% API  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(File0) ->
    wx:new(),
    macosx_workaround(),
    Frame = wx_object:start_link({local, ?MODULE}, ?MODULE, [args], []),
    put(top_frame, Frame),
    wxGLCanvas:setCurrent(?GET(gl_canvas)),
    {Frame, start_file(File0)}.

top_menus() ->
    Tail0 = [{?__(7,"Help"),help,wings_help:menu()}],
    Tail = case wings_pref:get_value(show_develop_menu) of
	       true ->
		   [{"Develop",develop,wings_develop:menu()}|Tail0];
	       false ->
		   Tail0
	   end,
    [{?__(1,"File"),  file,wings_file:menu()},
     {?__(2,"Edit"),  edit,wings:edit_menu()},
     {?__(3,"View"),  view,wings_view:menu()},
     {?__(4,"Select"),select,wings_sel_cmd:menu()},
     {?__(5,"Tools"), tools, wings:tools_menu()},
     {?__(6,"Window"),window,wings:window_menu()}|Tail].

make_external_win(Parent, Title, Opts0) ->
    FStyle = {style, ?wxCAPTION bor ?wxCLOSE_BOX bor ?wxRESIZE_BORDER},
    {Size, Opts} = case lists:keytake(size, 1, Opts0) of
		       {value, {size, Sz}, Os} -> {Sz, Os};
		       false  -> {false, Opts0}
		   end,
    Frame = wxMiniFrame:new(Parent, ?wxID_ANY, Title, [FStyle|Opts]),
    Size =/= false andalso wxWindow:setClientSize(Frame, Size),
    Frame.

register_win(Window) ->
    Frame = wx:typeCast(wxWindow:getParent(Window), wxMiniFrame),
    wx_object:call(?MODULE, {new_window, Frame, Window}).

get_icon_images() ->
    wx_object:call(?MODULE, get_images).

%%%%%%%% Internals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Inside wings (process)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

forward_event({current_state, #st{selmode=Mode, sh=Sh}}) ->
    wx_object:cast(?MODULE, {selmode, Mode, Sh}),
    keep;
forward_event({mode_restriction, _}=Restrict) ->
    wx_object:cast(?MODULE, Restrict),
    keep;
forward_event({menu, _}=Menu) ->
    wx_object:cast(?MODULE, Menu),
    keep;
forward_event({got_focus, _, _}=Focus) ->
    wx_object:cast(?MODULE, Focus),
    keep;
forward_event(_Ev) ->
    %% io:format("Dropped ~P~n", [_Ev, 20]),
    keep.

start_file(File0) ->
    %% On the Mac, if Wings was started by clicking on a .wings file,
    %% we must retrieve the name of the file here.
    Msgs0 = wxe_master:fetch_msgs(),
    Msgs = [F || F <- Msgs0, filelib:is_regular(F)],
    case Msgs of
	[F|_] -> F;
	[] -> File0
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Window in new (frame) process %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(state, {toolbar, windows, overlay, images}).
-record(split, {obj, mode, w1, w2}).

init(_Opts) ->
    try
	process_flag(trap_exit, true),
	wings_pref:set_default(window_size, {780,570}),
	TopSize = wings_pref:get_value(window_size),
	Frame = wxFrame:new(wx:null(), -1, "Wings 3D", [{size, TopSize}]),
	?SET(top_frame, Frame),
	set_icon(),
	Sizer = wxBoxSizer:new(?wxVERTICAL),
	Top = make(Frame),
	wxSizer:add(Sizer, win(Top), [{proportion, 1}, {flag, ?wxEXPAND}]),
	wxSizer:setSizeHints(Sizer, win(Top)),
	wxFrame:setSizer(Frame, Sizer),
	Canvas = wings_init:create(win(Top)),
	wxSplitterWindow:initialize(win(Top), Canvas),
	?SET(gl_canvas, Canvas),
	IconImgs = make_icons(),
	Toolbar = wings_toolbar:init(Frame, IconImgs),

	wxWindow:connect(Frame, close_window),
	wxWindow:connect(Frame, command_menu_selected, []),
	wxWindow:connect(Frame, activate, []),
	wxWindow:connect(Frame, show),
	wxFrame:show(Frame),
	init_menubar(Frame),
	%% Must be shown to initialize OpenGL context.
	receive #wx{obj=Frame, event=#wxShow{}} -> ok end,
	Wins = #{frame=>Frame, ch=>Top#split{w1=Canvas}, szr=>Sizer,
		 loose=>#{}, action=>undefined, op=>undefined},
	Overlay = overlay_frame(Frame),
	{Frame, #state{toolbar=Toolbar, images=IconImgs, windows=Wins, overlay=Overlay}}
    catch _:Reason ->
	    io:format("CRASH: ~p ~p ~p~n",[?MODULE, Reason, erlang:get_stacktrace()])
    end.

%%%%%%%%%%%%%%%%%%%%%%%

handle_event(#wx{obj=Win, event=#wxMove{}}, State) ->
    #wxMouseState{x=X,y=Y} = MS = wx_misc:getMouseState(),
    {noreply, preview_attach(stopped_moving(MS), {X,Y}, Win, State)};

handle_event(#wx{userData={move,Win}, event=Ev}, #state{windows=Wins0} = State) ->
    Wins = detach_window(Ev, Win, Wins0),
    {noreply, State#state{windows=Wins}};

handle_event(#wx{id=Id, event=#wxCommand{type=command_menu_selected}}, State) ->
    Name = wings_menu:id_to_name(Id),
    ME = case ets:match(wings_state, {{bindkey,'$1'}, Name, '_'}) of
	     [] -> {menubar, {action, Name}};
	     [[KeyComb]|_] -> wings_io_wx:make_key_event(KeyComb)
	 end,
    %% io:format("ME ~p~n",[ME]),
    wings ! ME,
    {noreply, State};
handle_event(#wx{event=#wxActivate{active=Active}}, State) ->
    Active == true andalso wxWindow:setFocus(?GET(gl_canvas)),
    wings ! #expose{active=Active},
    {noreply, State};
handle_event(#wx{event=#wxClose{}}, State) ->
    wings ! {quit},
    {noreply, State};
handle_event(Ev, State) ->
    io:format("~p:~p Got unexpected event ~p~n", [?MODULE,?LINE, Ev]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%

handle_call({new_window, Frame, Panel}, _From,
	    #state{windows=Wins=#{loose:=Loose}}=State) ->
    wxWindow:connect(Frame, move),
    {reply, ok, State#state{windows=Wins#{loose:=Loose#{Frame => Panel}}}};

handle_call(get_images, _From, #state{images=Icons} = State) ->
    {reply, Icons, State};

handle_call(Req, _From, State) ->
    io:format("~p:~p Got unexpected call ~p~n", [?MODULE,?LINE, Req]),
    {reply, ok, State}.

%%%%%%%%%%%%%%%%%%%%%%
handle_cast({selmode, _, _}=Sel, #state{toolbar=TB}=State) ->
    {noreply, State#state{toolbar=wings_toolbar:update(Sel, TB)}};
handle_cast({mode_restriction, _}=Restrict, #state{toolbar=TB}=State) ->
    {noreply, State#state{toolbar=wings_toolbar:update(Restrict, TB)}};
handle_cast({menu, {Menu, Key, Value}=Update}, #state{toolbar=TB}=State) ->
    wings_menu:update_menu_enabled(Menu, Key, Value),
    wings_toolbar:update(Update, TB),
    {noreply, State};
handle_cast({got_focus, _Window, Props}, #state{toolbar=TB}=State) ->
    Fun = fun(Menu, Key, Value) ->
		  wings_menu:update_menu_enabled(Menu, Key, Value),
		  wings_toolbar:update({Menu, Key, Value}, TB)
	  end,
    [Fun(view, Key, Value) || {Key, Value} <- Props, is_boolean(Value)],
    {noreply, State};
handle_cast(Req, State) ->
    io:format("~p:~p Got unexpected cast ~p~n", [?MODULE,?LINE, Req]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%
handle_info(check_stopped_move, #state{overlay=Overlay, windows=Wins0} = State) ->
    Wins = attach_floating(stopped_moving(wx_misc:getMouseState()), Overlay, Wins0),
    {noreply, State#state{windows=Wins}};
handle_info({close_window, Obj, _Str}, #state{windows=#{ch:=Child}=Wins} = State) ->
    Close = fun(Where, Other, GrandP) -> close_window(Obj, Where, Other, GrandP) end,
    case update_win(Obj, Child, Child, Close) of
	false -> error({child_no_exists, Obj});
	{ok, Root}  -> {noreply, State#state{windows=Wins#{ch:=Root}}}
    end;
handle_info(Msg, State) ->
    io:format("~p:~p Got unexpected info ~p~n", [?MODULE,?LINE, Msg]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%

code_change(_From, _To, State) ->
    State.

terminate(_Reason, #state{windows=#{frame:=Frame}}) ->
    io:format("~p: terminate: ~p~n",[?MODULE, _Reason]),
    catch wxFrame:destroy(Frame),
    normal.

%%%%%%%%%%%%%%%%%%%%%%
%% Window Management

preview_attach(false, Pos, Win,
	       #state{windows=#{action:=Action}=Wins, overlay=Overlay}=State)
  when Action =:= undefined; Action =:= preview_attach ->
    case get_split_path(Pos, Wins) of
	ignore -> %% Outside attached window
	    overlay_hide(Overlay),
	    State#state{windows=delete_timer(Wins)};
	Path ->
	    {Rect,_} = preview_rect(Path, Win),
	    overlay_draw(Overlay, Rect),
	    State#state{windows=setup_timer(Win, Path, delete_timer(Wins))}
    end;
preview_attach(_MouseDown, _Pos, _Win, State) ->
    %% There comes and initial move event when window is created
    %% ignore that
    State.

preview_rect({Obj, Path}, Win) ->
    Dir = lists:last(Path),
    {X,Y,W0,H0} = wxWindow:getScreenRect(win(Obj)),
    {MaxW, MaxH} = wxWindow:getSize(Win),
    W = min(W0 div 2, MaxW),
    H = min(H0 div 2, MaxH),
    case Dir of
	left  -> {{X,      Y, W, H0}, W-W0};
	right -> {{X+W0-W, Y, W, H0}, -W};
	up    -> {{X,      Y, W0, H}, H-H0};
	down  -> {{X, Y+H0-H, W0, H}, -H}
    end.

overlay_frame(Parent) ->
    Flags = ?wxFRAME_TOOL_WINDOW bor
	?wxFRAME_FLOAT_ON_PARENT bor
	?wxFRAME_NO_TASKBAR bor
	?wxNO_BORDER,
    Overlay = wxFrame:new(Parent, -1, "", [{style, Flags}]),
    wxFrame:setBackgroundColour(Overlay, {95,138,255, 200}),
    catch wxFrame:setTransparent(Overlay, 170),
    Overlay.

overlay_draw(Overlay, Rect) ->
    wxFrame:setSize(Overlay, Rect),
    wxFrame:show(Overlay).

overlay_hide(Overlay) ->
    wxFrame:hide(Overlay).

attach_floating(false, _, #{op:=Op}=State) when Op =/= undefined ->
    setup_timer(State);
attach_floating(true, Overlay, #{op:=#{mwin:=Container, mpath:=Path}, loose:=Loose}=State) ->
    case maps:get(Container, Loose, false) of
	false ->
	    State#{action:=undefined, op:=undefined};
	Window ->
	    overlay_hide(Overlay),
	    %% io:format("Attach window ~p ~p~n", [Container, Path]),
	    St = State#{loose:=maps:remove(Container,Loose), action:=undefined, op:=undefined},
	    DoWhileLocked = fun() -> attach_window(Path, Container, Window, St) end,
	    After = fun() -> timer:sleep(200), wings_io:reset_video_mode_for_gl(0,0) end,
	    wings_io:lock(whereis(wings), DoWhileLocked, After)
    end;
attach_floating(_B, _, State) ->
    %% Spurious Move events on windows
    State.

attach_window({_,Path}=Split, Container, NewWin, #{szr:=Sz, ch:=#split{obj=Parent}=Child} = State) ->
    Attach = fun() ->
		     Label = wxTopLevelWindow:getTitle(Container),
		     {_,Pos} = preview_rect(Split, NewWin),
		     Win = make_internal_win(Parent, NewWin, Label),
		     wxWindow:destroy(Container),
		     Root = split_win(Path, Win, Child, Pos),
		     case win(Root) =:= win(Child) of
			 false ->
			     wxSizer:replace(Sz, win(Child), win(Root)),
			     wxSizer:layout(Sz);
			 true -> ignore
		     end,
		     check_tree(Root, Child),
		     State#{ch:=Root}
	     end,
    wings_io:batch(Attach).

split_win([Dir], NewWin, Node, Pos) ->
    {Mode, Which} = split(Dir),
    case Node of
	#split{mode=undefined, w1=Root} -> %% Top Window
	    set_splitter(Which, Mode, Pos, Node, Root, NewWin);
	_Leaf ->
	    Parent = wxWindow:getParent(win(Node)),
	    NewNode = make(Parent),
	    reparent(NewWin, NewNode),
	    reparent(Node, NewNode),
	    set_splitter(Which, Mode, Pos, NewNode, Node, NewWin)
    end;
split_win([Which|Path], NewWin, #split{mode=Mode} = Node, Pos) ->
    {Child0, Child1} = child(Which, Node),
    Child = split_win(Path, NewWin, Child0, Pos),
    wxSplitterWindow:replaceWindow(win(Node), win(Child0), win(Child)),
    set_splitter(Which, Mode, false, Node, Child1, Child).

make(Parent0) ->
    Parent = win(Parent0),
    Style = case os:type() of
		{unix, darwin} -> ?wxSP_3DSASH bor ?wxSP_LIVE_UPDATE;
		_ -> ?wxSP_3D bor ?wxSP_LIVE_UPDATE
	    end,
    New = wxSplitterWindow:new(Parent, [{style, Style}]),
    %% wxSplitterWindow:setSashSize(New, 20),
    wxSplitterWindow:setSashGravity(New, 0.5),
    wxSplitterWindow:setMinimumPaneSize(New, 50),
    #split{obj=New}.

set_splitter(first, Mode, Pos, Sp, W2, W1) ->
    case Pos of
	false -> ignore;
	_ ->
	    wxSplitterWindow:Mode(win(Sp), win(W1), win(W2), [{sashPosition, Pos}])
    end,
    Sp#split{mode=Mode, w1=W1, w2=W2};
set_splitter(second, Mode, Pos, Sp, W1, W2) ->
    case Pos of
	false -> ignore;
	_ ->
	    wxSplitterWindow:Mode(win(Sp), win(W1), win(W2), [{sashPosition, Pos}])
    end,
    Sp#split{mode=Mode, w1=W1, w2=W2}.

reparent(#split{obj=This}=Child, #split{obj=Obj}) ->
    wxWindow:reparent(This, Obj),
    Child;
reparent(Child, #split{obj=Obj}) ->
    wxWindow:reparent(Child, Obj),
    Child.

stopped_moving(#wxMouseState{leftDown=L, middleDown=M, rightDown=R}=_MS) ->
    %% io:format("~p ~p ~p ~p~n",[L, M, R, _MS]),
    not (L orelse M orelse R).

get_split_path(ScreenPos, #{ch:=Child}) ->
    get_split_path(ScreenPos, Child, []).

get_split_path(Pos, #split{mode=undefined, w1=W1, w2=undefined}, Path) ->
    get_split_path(Pos, W1, Path);
get_split_path(Pos, #split{obj=Obj, mode=Mode, w1=W1, w2=W2}=Split, Path) ->
    Sash = wxSplitterWindow:getSashPosition(Obj),
    Win = wxWindow:getScreenRect(Obj),
    case is_inside(Pos, Win) of
	true ->
	    case get_split_op(Mode, Pos, Win, Sash) of
		first  -> get_split_path(Pos, W1, [first|Path]);
		second -> get_split_path(Pos, W2, [second|Path]);
		split  ->
		    Dir0 = get_split_op(other(Mode), Pos, Win, middle),
		    Dir = split(other(Mode),Dir0),
		    {Split, lists:reverse([Dir|Path])}
	    end;
	false ->
	    ignore
    end;
get_split_path(Pos, Leaf, Path) ->
    Win = wxWindow:getScreenRect(Leaf),
    case is_inside(Pos, Win) of
	true  -> {Leaf, lists:reverse([get_split_side(Pos, Win)|Path])};
	false -> ignore
    end.

-define(EPS, 0.02).
-define(MIN, (1.0 - ?EPS)).
-define(MAX, (1.0 + ?EPS)).
is_inside({PX,PY}, {WX,WY,WW,WH}) ->
    (WX * ?MIN) < PX andalso PX < (WX+WW)*?MAX
	andalso (WY * ?MIN) < PY andalso PY < (WY+WH)*?MAX.

get_split_op(splitHorizontally, {_,Y}, {_, WY, _, WW}, Sash) ->
    get_split_op2(Y, WY, WW, Sash);
get_split_op(splitVertically, {X, _}, {WX, _, WH, _}, Sash) ->
    get_split_op2(X, WX, WH, Sash).

get_split_op2(Pos, Top, Size, Sash) ->
    ScreenSash = case Sash of
		     middle -> Top + Size div 2;
		     _ -> Top + Sash
		 end,
    if abs(Pos - ScreenSash) < 20, Sash =/= middle -> split;
       Pos < ScreenSash -> first;
       true -> second
    end.

%% ___
%% |X| Figure out in which part of X we are
%% ---
get_split_side({PX,PY}, {AX,AY,W,H}) ->
    BX = AX+W, BY = AY+H,
    CX = AX,   CY = AY+H,
    DX = AX+W, DY = AY,
    case {((BX-AX)*(PY-AY)-(BY-AY)*(PX-AX)) > 0, ((DX-CX)*(PY-CY)-(DY-CY)*(PX-CX)) > 0} of
	{true, true}   -> down;
	{true, false}  -> left;
	{false, true}  -> right;
	{false, false} -> up
    end.



update_win(Win, #split{w1=Win, w2=Other}=Parent, GrandParent, Fun) ->
    Fun(Parent, Other, GrandParent);
update_win(Win, #split{w2=Win, w1=Other}=Parent, GrandParent, Fun) ->
    Fun(Parent, Other, GrandParent);
update_win(Win, #split{w1=W1, w2=W2}=Parent, _, Fun) ->
    case update_win(Win, W1, Parent, Fun) of
	false ->
	    case update_win(Win, W2, Parent, Fun) of
		false -> false;
		{Acc, UpdW2} -> {Acc, Parent#split{w2=UpdW2}}
	    end;
	{Acc, UpdW1} ->
	    {Acc, Parent#split{w1=UpdW1}}
    end;
update_win(_, _, _, _) -> false.

close_window(Delete, Split, Other, GrandP) ->
    wxWindow:reparent(win(Other), win(GrandP)),
    case GrandP of
	Split -> %% TopLevel
	    wxSplitterWindow:unsplit(win(GrandP), [{toRemove, Delete}]),
	    wxWindow:destroy(Delete),
	    {ok, GrandP#split{mode=undefined, w1=Other, w2=undefined}};
	#split{} ->
	    wxSplitterWindow:replaceWindow(win(GrandP), win(Split), win(Other)),
	    wxWindow:destroy(win(Split)),
	    {ok, Other}
    end.


detach_window(#wxMouse{type=motion, leftDown=true, x=X,y=Y}, #{bar:=Bar},
	      #{action:=detach_move, op:=#{frame:=Frame, disp:={XD,YD}}}=State) ->
    {X1,Y1} = wxWindow:clientToScreen(Bar, {X,Y}),
    wxFrame:move(Frame, X1-XD, Y1-YD),
    State;
detach_window(#wxMouse{type=motion, leftDown=true, x=X,y=Y},
	      #{bar:=Bar, win:=Container}=Win,
	      #{action:=detach_init, op:=#{pos:={X0,Y0}}=Op,
		loose:=Loose}=State) ->
    {X1,Y1} = wxWindow:clientToScreen(Bar, {X,Y}),
    case start_drag(abs(X1-X0), abs(Y1-Y0)) of
	false ->
	    %% io:format("Ignore: ~p~n",[?LINE]),
	    State;
	true  ->
	    {XB,YB} = wxWindow:getScreenPosition(Bar),
	    Displace = {X1-XB, Y1-YB},
	    WhileLocked = fun() ->
				  case wings_io:batch(fun() -> setup_detach(Win, State) end) of
				      {{true, Res}, Root} ->
					  receive
					      #wx{obj=Container, event=#wxShow{}} ->
						  {Res, Root}
					  end;
				      {{false, Res}, Root} ->
					  {Res, Root}
				  end
			  end,
	    {{Frame,NewWin}, Root} = wings_io:lock(whereis(wings), WhileLocked),
	    check_tree(Root, maps:get(ch, State)),
	    wxWindow:captureMouse(Bar),
	    State#{action:=detach_move,
		   op:=Op#{frame=>Frame, disp=>Displace},
		   ch:=Root, loose:=Loose#{Frame=>NewWin}}
    end;
detach_window(#wxMouse{type=left_down, x=X, y=Y}, #{bar:=Bar}=Info, State) ->
    Pos = wxWindow:clientToScreen(Bar, {X,Y}),
    State#{action:=detach_init, op:=#{win=>Info, pos=>Pos}};
detach_window(#wxMouse{type=left_up}, #{bar:=Bar, win:=Win}, #{action:=Action}=State) ->
    case Action of
	detach_move ->
	    ok = wxWindow:releaseMouse(Bar),
	    wxWindow:destroy(Win);
	_ -> ignore
    end,
    State#{action:=undefined, op:=undefined};
detach_window(_Ev, _, State) ->
    %% io:format("Ignore: ~p~n",[_Ev]),
    State.

setup_detach(#{win:=Container}=Win, #{szr:=Szr, ch:=Child, frame:=Parent}) ->
    Detach = fun(Where, Other, GrandP) ->
		     Res = do_detach_window(Win, Where, Other, GrandP, Parent, Szr),
		     wings ! {wm, dirty},
		     Res
	     end,
    Res = {_, Root} = update_win(Container, Child, top, Detach),
    case win(Root) =:= win(Child) of
	false -> wxSizer:layout(Szr);
	true ->  ignore
    end,
    Res.

do_detach_window(#{win:=Container, child:=Child, label:=Label}, Split, Other, GrandP, Top, Szr) ->
    PosSz = wxWindow:getScreenRect(Container),
    wxWindow:setSize(Container, {-1,-1,1,1}),
    wxWindow:reparent(Container, Top),
    Frame = make_external_win(Top, Child, PosSz, Label),
    wxWindow:hide(Container),
    Sizer = wxWindow:getSizer(Container),
    wxSizer:detach(Sizer, Child),
    case GrandP of
	top when is_record(Other, split) ->
	    wxWindow:reparent(win(Other), Top),
	    wxSizer:replace(Szr, win(Split), win(Other)),
	    wxWindow:destroy(win(Split)),
	    {{show_detached(Container), Frame}, Other};
	top ->
	    wxWindow:reparent(win(Other), win(Split)),
	    wxSplitterWindow:unsplit(win(Split), [{toRemove, Container}]),
	    {{show_detached(Container), Frame},
	     Split#split{mode=undefined, w1=Other, w2=undefined}};
	#split{} ->
	    wxWindow:reparent(win(Other), win(GrandP)),
	    wxSplitterWindow:replaceWindow(win(GrandP), win(Split), win(Other)),
	    wxWindow:destroy(win(Split)),
	    {{show_detached(Container), Frame}, Other}
    end.

show_detached(Window) ->
    case os:type() of
	{unix, linux} ->
	    %% We are grabbing mouse events to move the detached window
	    %% but on linux/gtk we can not grab hidden or non realized
	    %% windows so we sync on show to before we grab
	    wxWindow:connect(Window, show),
	    wxWindow:show(Window),
	    true;
	_ ->
	    false
    end.

start_drag(DX, DY) ->
    (DX > wxSystemSettings:getMetric(?wxSYS_DRAG_X))
	orelse (DY > wxSystemSettings:getMetric(?wxSYS_DRAG_Y)).

make_external_win(Parent, Child, {X0,Y0, W, H} = _Dim, Label) ->
    Frame = make_external_win(Parent, Label, [{pos, {X0,Y0}}, {size, {W,H}}]),
    wxWindow:connect(Frame, move),
    wxWindow:reparent(Child, Frame),
    wxWindow:refresh(Child),
    wxWindow:update(Child),
    wxWindow:show(Frame),
    {Frame, Child}.

split(left)  -> {splitVertically, first};
split(right) -> {splitVertically, second};
split(up)    -> {splitHorizontally, first};
split(down)  -> {splitHorizontally, second}.

split(splitVertically, first) -> left;
split(splitVertically, second) -> right;
split(splitHorizontally, first) -> up;
split(splitHorizontally, second) -> down.

other(splitHorizontally) -> splitVertically;
other(splitVertically) -> splitHorizontally.

child(first,  #split{w1=W1, w2=W2}) -> {W1, W2};
child(second, #split{w1=W1, w2=W2}) -> {W2, W1}.

win(#split{obj=Obj}) -> Obj;
win(Obj) -> Obj.

delete_timer(#{op:= #{mtimer:=Ref}}=St)
  when Ref =/= undefined ->
    timer:cancel(Ref),
    St#{action:=undefined, op:=undefined};
delete_timer(St) ->
    St.

setup_timer(Win, Path, St) ->
    setup_timer(St#{action:=preview_attach, op=>#{mwin=>Win, mpath=>Path}}).
setup_timer(#{op:=Op} = St) ->
    {ok, TRef} = timer:send_after(200, check_stopped_move),
    St#{op:=Op#{mtimer=>TRef}}.

-define(WIN_BAR_HEIGHT, 20).

make_internal_win(Parent, ChildWin, Label) ->
    Win = wxPanel:new(Parent, []),
    [Bar|_] = Wins = make_bar(Win, Label),
    Top = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Top, Bar, [{flag, ?wxEXPAND}]),
    wxWindow:reparent(ChildWin, Win), %% Send event here
    wxSizer:add(Top, ChildWin,  [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxWindow:setSizer(Win, Top),
    Info = #{bar=>Bar, win=>Win, child=>ChildWin, label=>Label},
    [wxWindow:connect(W, Ev, [{userData, {move, Info}}])
     || Ev <- [left_down, left_up, motion], W <- Wins],
    Win.

make_bar(Parent, Label) ->
    Bar = wxPanel:new(Parent, [{size, {-1, ?WIN_BAR_HEIGHT}}]),
    BG = wings_pref:get_value(title_passive_color),
    FG = wings_pref:get_value(title_text_color),
    wxPanel:setFont(Bar, ?GET(system_font_wx)),
    wxWindow:setBackgroundColour(Bar, wings_color:rgb4bv(BG)),
    wxWindow:setForegroundColour(Bar, wings_color:rgb4bv(FG)),
    WBSz = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:addSpacer(WBSz, 20),
    wxSizer:add(WBSz, ST=wxStaticText:new(Bar, ?wxID_ANY, Label), [{flag, ?wxALIGN_CENTER}]),
    wxSizer:addStretchSpacer(WBSz),
    Bitmap = wxArtProvider:getBitmap("wxART_CROSS_MARK",[{client, "wxART_FRAME_ICON"}]),
    SBM = wxStaticBitmap:new(Bar, ?wxID_EXIT, Bitmap),
    wxSizer:add(WBSz, SBM, [{flag, ?wxALIGN_CENTER}]),
    wxSizer:addSpacer(WBSz, 5),
    wxWindow:setSizer(Bar, WBSz),
    Self = self(),
    CB = fun(_, Ev) ->
		 wxMouseEvent:skip(Ev),
		 Self ! {close_window, Parent, Label}
	 end,
    wxWindow:connect(SBM, left_up, [{callback, CB}]),
    [Bar,ST].

check_tree(#split{} = T, Orig) ->
    try
	true = tree_consistent(T)
    catch _:Reason ->
	    io:format("Failed ~p ~p~n", [Reason, erlang:get_stacktrace()]),
	    io:format("Orig ~p~nNew ~p~n",[Orig, T])
    end;
check_tree(T, Orig) ->
    io:format("Failed no root ~n", []),
    io:format("Orig ~p~nNew ~p~n",[Orig, T]).

tree_consistent(#split{w1=W1, mode=undefined, w2=undefined})
  when wx_ref =:= element(1, W1) ->
    true;
tree_consistent(#split{w1=W1, mode=Mode, w2=W2})
  when Mode =/= undefined ->
    tree_consistent(W1) andalso tree_consistent(W2);
tree_consistent(Leaf)
  when wx_ref =:= element(1, Leaf) ->
    true.

%%%%%%%%%%%%%%%%%%%%%%
set_icon() ->
    Ebin = filename:dirname(code:which(?MODULE)),
    IconFile = filename:join(Ebin, "wings_icon_379x379"),
    wings_io:set_icon(IconFile).

macosx_workaround() ->
    try 1.0/zero()
    catch
	error:_ -> ok
    end.

zero() ->
    0.0.

%% Returns a list of wxImages
make_icons() ->
    MakeImage = fun({Name, {Bpp, W, H, Bin0}}) ->
			{Colors, Alpha} = setup_image(Bin0, Bpp, W),
			Image = wxImage:new(W,H,Colors),
			wxImage:setAlpha(Image, Alpha),
			{Name, {W,H}, Image}
		end,
    [MakeImage(Raw) || Raw <- binary_to_term(wings_io:read_icons())].

%% FIXME when icons are fixed
%% Poor mans version of alpha channel
setup_image(Bin0, 3, Width) ->
    RowLen = 3*Width,
    Bin = iolist_to_binary(lists:reverse([Row || <<Row:RowLen/binary>> <= Bin0])),
    rgb3(Bin, <<>>, <<>>);
setup_image(Bin0, 4, Width) ->
    RowLen = 4*Width,
    Bin = iolist_to_binary(lists:reverse([Row || <<Row:RowLen/binary>> <= Bin0])),
    rgb4(Bin, <<>>, <<>>).

rgb3(<<8684676:24, Rest/binary>>, Cs, As) ->
    rgb3(Rest, <<Cs/binary, 8684676:24>>, <<As/binary, 0:8>>);
rgb3(<<R:8,G:8,B:8, Rest/binary>>, Cs, As) ->
    A0 = abs(R-132)/123,
    A1 = abs(G-132)/123,
    A2 = abs(B-132)/123,
    A = trunc(255*min(1.0, max(max(A0,A1),A2))),
    rgb3(Rest, <<Cs/binary, R:8, G:8, B:8>>, <<As/binary, A:8>>);
rgb3(<<>>, Cs, As) ->
    {Cs,As}.

rgb4(<<C:24, A:8, R/binary>>, Cs, As) ->
    rgb4(R, <<Cs/binary, C:24>>, <<As/binary, A:8>>);
rgb4(<<>>, Cs, As) ->
    {Cs,As}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Menubar

init_menubar(Frame) ->
    ets:new(wings_menus, [named_table, public, {keypos,2}]),
    put(wm_active, {menubar, geom}),
    MB = wxMenuBar:new(),
    try
	wings_menu:setup_menus(MB, top_menus()),
	wxFrame:setMenuBar(Frame, MB),
	ok
    catch _ : Reason ->
	    io:format("CRASH ~p ~p~n",[Reason, erlang:get_stacktrace()]),
	    error(Reason)
    end,
    erase(wm_active),
    ok.
