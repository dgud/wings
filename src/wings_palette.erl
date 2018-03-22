%%
%%  wings_palette.erl --
%%
%%     Maintains the vertex palette window.
%%
%%  Copyright (c) 2004-2011 Bjorn Gustavsson, Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
-module(wings_palette).

-export([window/1,window/4]).
-export([palette/1]).

%% Internal
-export([init/1,
	 handle_call/3, handle_cast/2, handle_event/2, handle_info/2,
	 code_change/3, terminate/2]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [foldl/3,reverse/1,keyfind/3]).

-define(BOX_W, 20).
-define(BOX_H, 20).

-define(COLS_W, 8).
-define(COLS_H, 8).

-define(BORD, 2).

-record(state, {self, win, sz, bsz, cols, timer, empty}).

-behaviour(wx_object).

%% API  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

window(St) ->
    case wings_wm:is_window(palette) of
	true ->
	    wings_wm:raise(palette),
	    keep;
	false ->
	    {DeskW,_DeskH} = wings_wm:top_size(),
	    Pos  = {DeskW-50, 0},
	    Size = {?COLS_W*?BOX_W+?COLS_W*?BORD+?BORD*2,
		    ?COLS_H*?BOX_H+?COLS_H*?BORD+?BORD*2},
	    create_window(Pos, Size, [], St),
	    keep
    end.

window(Pos, Size, Ps, St) ->
    create_window(Pos, Size, Ps, St).

palette(#st{pal=[]}) -> [];
palette(#st{pal=Pal0}) ->
    Pal = del_trailing(Pal0),
    case Pal == default_cols() of
	true -> [];
	false -> Pal
    end.

%%%%%%%% Palette internals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Inside wings (process)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_window(Pos, Size, Ps0, St) ->
    Cols  = get_all_colors(St),
    {Frame,Ps} = wings_frame:make_win(title(), [{size, Size}, {pos, Pos}|Ps0]),
    Window = wings_sup:window(undefined, ?MODULE, [Frame, Size, Ps, Cols]),
    F = fun({color,_}) -> yes;
	   ({material, _}) -> yes;
	   (_) -> no
	end,
    Fs = [{drag_filter, F}, {display_data, geom_display_lists}|Ps],
    wings_wm:toplevel(palette, Window, Fs, {push,change_state(Window, St)}),
    keep.

change_state(Window, St) ->
    fun(Ev) -> forward_event(Ev, Window, St) end.

forward_event(redraw, _Window, _St) -> keep;
forward_event({current_state, _, _}, _Window, _St0) -> keep;
forward_event({current_state, #st{pal=Cols}=St}, Window, #st{pal=Cols0}) ->
    (Cols =:= Cols0) orelse wx_object:cast(Window, {new_state, Cols}),
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

title() ->
    ?__(1,"Palette").

default_cols() ->
    [{1.0,1.0,1.0},{0.8,1.0,0.8},{0.8,1.0,1.0},{0.8,0.8,1.0},{0.9,1.0,0.8},
     {1.0,0.8,0.5},{1.0,0.8,0.8},{0.0,0.0,0.0},{0.63922,0.4,0.0},
     {0.8,0.6,0.0},{1.0,0.8,0.0},{1.0,1.0,0.00392157},{0.996078,1.0,0.6},
     {1.0,0.858824,0.615686},{1.0,0.8,0.4},{1.0,0.6,0.23922},
     {1.0,0.474510,0.290196},{1.0,0.2,0.0},{0.63922,0.0,0.0},
     {0.2,0.2,0.43922},{0.0,0.2,0.6},{0.0,0.4,0.796078},
     {0.0,0.59804,0.839216},{0.0,0.6,1.0},{0.243137,0.6,0.874510},
     {0.6,0.83922,1.0},{0.79804,0.886275,1.0},{0.874510,0.996078,1.0},
     {0.996078,0.8,1.0},{0.8,0.8,1.0},{0.63922,0.6,1.0},{0.43922,0.4,0.8},
     {0.6,0.6,0.83922},{0.4,0.4,0.63922},{0.0,0.4,0.0},{0.0,0.6,0.0},
     {0.43922,0.796078,0.2},{0.63922,0.996078,0.4},{0.83922,1.0,0.8}].

get_all_colors(#st{pal=Pal}) ->
    case Pal of
	[] ->
	    Def = default_cols(),
	    Def ++ lists:duplicate(?COLS_W*?COLS_H-length(Def), none);
	Cols ->
	    Cols
    end.


do_menu(Win, Id, Pos, Cols) ->
    Menu = [{?__(1,"Edit"),{'VALUE',{edit,Id}},?__(2,"Edit color")}],
    Smooth = case lists:nth(Id, Cols) of
		 none ->
		     [{?__(3,"Interpolate"),{'VALUE',{smooth,Id}},
		       ?__(4,"Interpolate Empty Colors")}];
		 _ -> []
	     end,
    Rest = [separator,
	    {?__(5,"Clear All"), clear_all,?__(6,"Clear palette")},
	    {?__(7,"Compact"), compact,?__(8,"Compact Palette")},
	    {?__(9,"Scan Colors"), scan_all, ?__(10,"Scan colors from selection")},
	    separator,
	    {?__(11,"Export"), export,?__(12,"Export palette to file")},
	    {?__(13,"Import"), import,?__(14,"Import palette from file")}],
    wings_menu:popup_menu(Win,Pos,palette,Menu ++ Smooth ++ Rest).

write_file(Name, Cols) ->
    case file:open(Name, [write]) of
	{ok,F} ->
	    io:format(F, "%% Wings Palette~n", []),
	    io:format(F, "{palette, ~p}.~n", [del_trailing(Cols)]),
	    file:close(F);
	Error -> Error
    end.

scan_colors(#st{mat=Mtab, selmode=Mode}=St, Cols0) ->
    Cols1 = scan_materials(gb_trees:values(Mtab), Cols0),
    Cols2 = lists:usort(Cols1),
    MF = fun(Items, We) ->
		 Colors = scan_color(Mode, Items, We),
		 lists:usort(Colors)
	 end,
    RF = fun lists:umerge/2,
    wings_sel:dfold(MF, RF, Cols2, St).

scan_color(body, _, We) ->
    wings_va:all(color, We);
scan_color(vertex, Vs, We) ->
    F = fun(V, A) ->
	       VsFun = fun(_, Face, _, Acc) ->
			       Attr = wings_va:vtx_attrs(V, Face, We),
			       add_cols([Attr], Acc)
		       end,
		wings_vertex:fold(VsFun, A, V, We)
	end,
    gb_sets:fold(F, [], Vs);
scan_color(edge, Edges, #we{es=Etab}=We) ->
    gb_sets:fold(
      fun(Edge, Acc) ->
	      #edge{lf=LF,rf=RF,ve=Ve,vs=Vs} = array:get(Edge, Etab),
	      A = wings_va:vtx_attrs(Vs, LF, We),
	      B = wings_va:vtx_attrs(Vs, RF, We),
	      C = wings_va:vtx_attrs(Ve, LF, We),
	      D = wings_va:vtx_attrs(Ve, RF, We),
	      add_cols([A,B,C,D], Acc)
      end, [], Edges);
scan_color(face, Faces, We) ->
    gb_sets:fold(
      fun(Face, Acc) ->
	      Attrs = wings_va:face_attr(color, Face, We),
	      add_cols(Attrs, Acc)
      end, [], Faces).

scan_materials([Mat|Ms], Cols) ->
    Opengl = proplists:get_value(opengl, Mat),
    Diff   = proplists:get_value(diffuse, Opengl),
    Emis   = proplists:get_value(emission, Opengl),
    scan_materials(Ms, [color(Diff),color(Emis)|Cols]);
scan_materials([], Cols) -> Cols.

add_cols([none|R], Acc) ->
    add_cols(R,Acc);
add_cols([Col|R], Acc) when tuple_size(Col) =:= 3 ->
    add_cols(R, [Col|Acc]);
add_cols([Attr|R], Acc) ->
    add_cols(R, [wings_va:attr(color, Attr)|Acc]);
add_cols([], Acc) -> Acc.

import(St, Cols0) ->
    Dir = wings_pref:get_value(current_directory),
    Ps = [{title,?__(7,"Import")},{directory,Dir},
	  {ext,".wpal"},{ext_desc,?__(9,"Wings Palette")}],
    Fun = fun(Name) ->
		  case file:consult(Name) of
		      {ok,Content} ->
			  case keyfind(palette,1,Content) of
			      {palette, Pal} when is_list(Pal) ->
				  Cols = del_trailing(Cols0) ++ Pal,
				  wings_wm:send(geom, {new_state,St#st{pal=Cols}}),
				  keep;
			      _ ->
				  Reason = ?__(10,"No palette found"),
				  Msg = io_lib:format(?__(11,"Import error: ~w"), [Reason]),
				  wings_u:message(Msg),
				  keep
			  end;
		      {error,Reason} ->
			  Msg = io_lib:format(?__(12,"Import error: ~w"), [Reason]),
			  wings_u:message(Msg),
			  keep
		  end
	  end,
    wings_plugin:call_ui({file,open_dialog,Ps,Fun}).

export(Cols) ->
    Dir = wings_pref:get_value(current_directory),
    Ps = [{title,?__(3,"Export")},{directory,Dir},
	  {ext,".wpal"},{ext_desc,?__(5,"Wings Palette")}],
    Fun = fun(Name) ->
		  case write_file(Name, Cols) of
		      ok -> keep;
		      {error,Reason} ->
			  Msg = io_lib:format(?__(6,"Export error: ~w"), [Reason]),
			  wings_u:message(Msg),
			  keep
		  end
	  end,
    wings_plugin:call_ui({file,save_dialog,Ps,Fun}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Window in new process %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Frame, {W,_}, _Ps, Cols0]) ->
    try
	Empty = make_bitmap(none),
	Tmp = make_button(Frame, -1, none, Empty),
	BSz = {BW,_} = wxWindow:getSize(Tmp),
	wxWindow:destroy(Tmp),
	{ColsW,ColsH} = calc_size(Cols0,W,BW,false),
	%% io:format("Init Size ~p => ~p~n",[BSz, {ColsW, ColsH}]),
	Win = wxScrolledWindow:new(Frame),
	Sz = wxGridSizer:new(ColsW, [{vgap, ?BORD},{hgap, ?BORD}]),
	Cols = add_empty(Cols0,ColsW,ColsH),
	manage_bitmaps(Win, Sz, Cols, [], Empty),
	BorderSz = wxBoxSizer:new(?wxHORIZONTAL),
	wxSizer:add(BorderSz, Sz,
	 	    [{border, 4},{proportion, 1},
	 	     {flag, ?wxALL bor ?wxEXPAND}]),
	wxWindow:setSizer(Win, BorderSz),
	wxScrolledWindow:setScrollRate(Win, 0, ?BOX_H+?BORD),
	wxWindow:connect(Win, size, [{skip, true}]),
	wxWindow:connect(Win, enter_window, [{userData, {win, Win}}]),
	wings_status:message(palette, help()),
	{Win, #state{self=self(), win=Win, sz=Sz,
		     cols=Cols, bsz=BSz, empty=Empty}}
    catch _:Reason ->
	    io:format("CRASH: ~p ~p ~p~n",[?MODULE, Reason, erlang:get_stacktrace()]),
            error(Reason)
    end.

%%%%%%%%%%%%%%%%%%%%%%%
handle_event(#wx{event=#wxMouse{type=enter_window}}=Ev, State) ->
    wings_frame ! Ev,
    {noreply, State};
handle_event(#wx{event=#wxMouse{type=left_down}}, State) ->
    {noreply, State};
handle_event(#wx{id=Id, event=#wxMouse{type=left_up, controlDown=true}},
	     #state{cols=Cols0, empty=Empty, win=Win} = State) ->
    %% Clear color
    {Bef,[_Prev|Rest]} = lists:split(Id-1, Cols0),
    Cols = Bef ++ [none|Rest],
    setColor(Id, none, Empty, Win),
    {noreply, update(Cols, State)};
handle_event(#wx{id=Id, event=#wxMouse{type=middle_up}}, State) ->
    {noreply, set_color(Id, State)};
handle_event(#wx{id=Id, event=#wxMouse{type=left_up}}, #state{cols=Cols} = State) ->
    case lists:nth(Id, Cols) of
	none ->
	    {noreply, set_color(Id, State)};
	Color ->
	    Set = fun(St0) ->
			  St = case St0#st.selmode of
				   vertex ->
				       wings_vertex_cmd:set_color(Color, St0);
				   edge ->
				       wings_edge_cmd:set_color(Color, St0);
				   face ->
				       wings_face_cmd:set_color(Color, St0);
				   body ->
				       St1 = wings_sel_conv:mode(face, St0),
				       St2 = wings_face_cmd:set_color(Color, St1),
				       St2#st{sel=St0#st.sel,selmode=St0#st.selmode}
			       end,
			  wings_wm:send(geom, {new_state,St}),
			  St
		  end,
	    wings_wm:psend(palette, {apply, true, Set}),
	    {noreply, State}
    end;

handle_event(Ev=#wx{id=Id, event=#wxMouse{}}, #state{cols=Cols, win=Win}=State) ->
    case wings_menu:is_popup_event(Ev) of
	{yes, GlobalPos} ->
	    wings_wm:psend(palette, {apply, false, fun(_) -> do_menu(Win, Id, GlobalPos, Cols) end});
	_ ->
	    %% io:format("Ignored ~p~n",[Ev]),
	    ok
    end,
    {noreply, State};

handle_event(#wx{event=#wxSize{size=Sz}}, #state{timer=TRef} = State) ->
    TRef =/= undefined andalso timer:cancel(TRef),
    {ok, Timer} = timer:send_after(150, {resize, Sz}),
    {noreply, State#state{timer=Timer}};
handle_event(_Ev, State) ->
    %% io:format("~p:~p Got unexpected event ~p~n", [?MODULE,?LINE, _Ev]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%

handle_call(_Req, _From, State) ->
    %% io:format("~p:~p Got unexpected call ~p~n", [?MODULE,?LINE, _Req]),
    {reply, ok, State}.

%%%%%%%%%%%%%%%%%%%%%%

handle_cast({action, {palette, {edit, Id}}}, State) ->
    {noreply, set_color(Id, State)};

handle_cast({action, {palette, {smooth, Id0}}},
	    #state{cols=Cols0, empty=Empty, win=Win} = State) ->
    {Bef0,After0} = lists:split(Id0-1, Cols0),
    {BC, Bef1} = del_empty(reverse(Bef0)),
    {AC, Aft1} = del_empty(After0),
    case interpolate(Bef1,Aft1,AC+BC) of
	no_start ->
	    wings_u:message(?__(1,"No start color found.")),
	    {noreply, State};
	no_end ->
	    wings_u:message(?__(2,"No end color found.")),
	    {noreply, State};
	IntCols ->
	    Update = fun(Col, Id) -> setColor(Id, Col, Empty, Win), Id+1 end,
	    lists:foldl(Update, length(Bef1)+1, IntCols),
	    Cols = reverse(Bef1) ++ IntCols ++ Aft1,
	    {noreply, update(Cols, State)}
    end;

handle_cast({action, {palette, clear_all}},
	    #state{cols=Cols0, empty=Empty, win=Win} = State) ->
    Cols = lists:duplicate(length(Cols0), none),
    Update = fun(Col, Id) -> setColor(Id, Col, Empty, Win), Id+1 end,
    lists:foldl(Update, 1, Cols),
    {noreply, update(Cols, State)};

handle_cast({action, {palette, compact}},
	    #state{cols=Cols0, empty=Empty, win=Win} = State) ->
    Cols1 = foldl(fun(none,Acc) -> Acc;
		     (Col, Acc) -> [Col|Acc]
		  end, [], Cols0),
    Cols = add_empty(reverse(Cols1), length(Cols0)),
    Update = fun(Col, Id) -> setColor(Id, Col, Empty, Win), Id+1 end,
    lists:foldl(Update, 1, Cols),
    {noreply, update(Cols, State)};

handle_cast({action, {palette, scan_all}}, #state{cols=Cols}=State) ->
    Scan = fun(St0) ->
		   New = scan_colors(St0, del_trailing(Cols)),
		   St = St0#st{pal=New},
		   wings_wm:send(geom, {new_state,St}),
		   St0  %% Intentional so we get updates to window process
	   end,
    wings_wm:psend(palette, {apply, true, Scan}),
    {noreply, State};

handle_cast({action, {palette, import}}, #state{cols=Cols} = State) ->
    Import = fun(St) -> import(St, Cols) end,
    wings_wm:psend(palette, {apply, false, Import}),
    {noreply, State};

handle_cast({action, {palette, export}}, #state{cols=Cols} = State) ->
    Export = fun(_) -> export(Cols) end,
    wings_wm:psend(palette, {apply, false, Export}),
    {noreply, State};

handle_cast({new_state, Cols}, #state{cols=Cols}=State) ->
    {noreply, State};
handle_cast({new_state, StColors},
	    #state{sz=Sizer, cols=OldCs, empty=Empty, win=Win}=State) ->
    %% io:format("NewState ~p ~p~n",[length(StColors), length(OldCs)]),
    NewSz = length(StColors),
    OldSz = length(OldCs),
    Cols = case NewSz =< OldSz of
	       true ->
		   add_empty(StColors, OldSz);
	       false ->
		   CW = wxGridSizer:getCols(Sizer),
		   CH = if (NewSz rem CW) > 0 ->
				1 + (NewSz div CW);
			   true ->
				NewSz div CW
			end,
		   WithEmpty = add_empty(StColors, CW, CH),
		   %% io:format("~p => ~p ~p ~n", [NewSz, CW, CH]),
		   manage_bitmaps(Win, Sizer, WithEmpty, OldCs, Empty),
		   WithEmpty
	   end,
    %% io:format("Length of new ~p ~n", [length(Cols)]),
    Update = fun(Col, Id) -> setColor(Id, Col, Empty, Win), Id+1 end,
    lists:foldl(Update, 1, Cols),
    {noreply, State#state{cols=Cols}};

handle_cast(_Req, State) ->
    %% io:format("~p:~p Got unexpected cast ~p~n", [?MODULE,?LINE, _Req]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%

handle_info(Msg={resize, Size}, State0) ->
    case wx_misc:getMouseState() of
	#wxMouseState{leftDown=true} ->
	    {ok, Ref} = timer:send_after(250, Msg),
	    {noreply, State0#state{timer=Ref}};
	_ ->
	    State = resize(Size, State0),
	    {noreply, State#state{timer=undefined}}
    end;
handle_info(_Msg, State) ->
    %io:format("~p:~p Got unexpected info ~p~n", [?MODULE,?LINE, _Msg]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%

code_change(_From, _To, State) ->
    State.

terminate(_Reason, _) ->
    %io:format("terminate: ~p (~p)~n",[?MODULE, _Reason]),
    wings ! {wm, {delete, palette}},
    normal.

%%%%%%%%%%%%%%%%%%%%%%

help() -> 
    L = wings_msg:button_format(?__(1,"Assign color to selection")),
    MR = wings_msg:button_format([],
				 ?__(2,"Edit color"),
				 ?__(3,"Show menu")),
    Mods = wings_msg:free_modifier(),
    ModName = wings_msg:mod_name(Mods),
    CL = [ModName,$+,wings_msg:button_format(?__(4,"Clear color"))],
    wings_msg:join([L,CL,MR]).

resize({W,H}, #state{cols=Cols0, bsz={BW,BH}, empty=Empty, win=Win, sz=Sizer} = State) ->
    Cols1 = del_trailing(Cols0),
    {ColsW0, ColsH0} = calc_size(Cols1, W, BW, false),
    Visible = (H-5) div (BH+?BORD),
    {ColsW,ColsH} =
	if Visible < ColsH0, (W - ColsW0 * BW) < 20 ->
		%% Reduce a column for scrollbar
		calc_size(Cols1, W, BW, true);
	   Visible < ColsH0 ->
		{ColsW0, ColsH0};
	   true ->
		{ColsW0, Visible}
	end,
    %% io:format("Size ~p (~p) => ~p~n",[{BW,BH}, Visible < ColsH0, {ColsW, ColsH}]),
    Cols = add_empty(Cols1,ColsW,ColsH),
    wxGridSizer:setCols(Sizer, ColsW),
    manage_bitmaps(Win, Sizer, Cols, Cols0, Empty),
    State#state{cols=Cols}.

manage_bitmaps(Win, Sz, Cols, Old, Empty) ->
    LenCols = length(Cols),
    LenOld  = length(Old),
    if LenCols =:= LenOld ->
	    ignore;
       LenCols > LenOld ->
	    wxWindow:freeze(Win),
	    wx:foldl(fun(_Col, Id) when Id =< LenOld ->
			     Id+1;
			(Col, Id) ->
			     wxSizer:add(Sz, make_button(Win, Id, Col, Empty)),
			     Id+1
		     end, 1, Cols),
	    wxWindow:thaw(Win),
	    ok;
       LenCols < LenOld ->
	    Cleanup = fun(Id) ->
			      StBM = wxSizerItem:getWindow(wxSizer:getItem(Sz, Id)),
			      wxSizer:remove(Sz, Id),
			      wxWindow:destroy(StBM)
		      end,
	    wx:foreach(Cleanup, lists:seq(LenOld-1, LenCols, -1))
    end,
    wxSizer:fitInside(Sz, Win),
    ok.

make_button(Parent, Id, FloatCol, Empty) ->
    {Delete, BM} = case FloatCol of
		       none -> {false, Empty};
		       _ -> {true, make_bitmap(FloatCol)}
		   end,
    Static = case os:type() of
		 {win32, _} -> wxBitmapButton:new(Parent, Id, BM, [{style, ?wxBORDER_NONE}]);
		 {_, _}     -> wxStaticBitmap:new(Parent, Id, BM)
	     end,
    Delete andalso wxBitmap:destroy(BM),
    [wxWindow:connect(Static, Ev, [{skip, false}]) ||
	Ev <- [left_down, left_up, left_dclick, middle_up, right_up]],
    Static.

set_color(Id, #state{win=Win, cols=Cols0, empty=Empty} = State) ->
    {Bef,[Prev|Rest]} = lists:split(Id-1, Cols0),
    Result = fun(Color) -> {return, Color} end,
    case wings_color:choose(color(Prev), Result) of
	keep ->
	    State;
	Color ->
	    Cols = Bef ++ [Color|Rest],
	    setColor(Id, Color, Empty, Win),
	    update(Cols, State)
    end.

setColor(Id, none, Empty, Parent) ->
    setBitmap(Id, Empty, Parent);
setColor(Id, Color, _, Parent) ->
    Bitmap = make_bitmap(Color),
    setBitmap(Id, Bitmap, Parent),
    wxBitmap:destroy(Bitmap).

setBitmap(Id, Bitmap, Parent) ->
    Win = wxWindow:findWindow(Parent, Id),
    wx:is_null(Win) andalso error({no_such_id, Id}),
    case os:type() of
	{win32, _} -> wxBitmapButton:setBitmapLabel(wx:typeCast(Win, wxBitmapButton), Bitmap);
	{_, _}     -> wxStaticBitmap:setBitmap(wx:typeCast(Win, wxStaticBitmap), Bitmap)
    end.

make_bitmap(Col0) ->
    Brush = case Col0 of
		none -> wxBrush:new({255,255,255,0});
		FloatC -> wxBrush:new(wings_color:rgb3bv(FloatC))
	    end,
    {BOX_H, BOX_W} = {?BOX_H-3,?BOX_W-3},
    BM = wxBitmap:new(BOX_H,BOX_W),
    DC = wxMemoryDC:new(),
    wxMemoryDC:selectObject(DC, BM),
    wxDC:clear(DC),
    wxDC:setBrush(DC, Brush),
    wxDC:setPen(DC, ?wxBLACK_PEN),
    wxDC:drawRoundedRectangle(DC, {0,0, BOX_H,BOX_W}, 2),
    Col0 == none andalso
	wxDC:drawLine(DC, {3,3}, {BOX_H-3,BOX_W-3}),
    wxMemoryDC:destroy(DC),
    wxBrush:destroy(Brush),
    BM.


del_trailing(L) ->
    {_,R} = del_empty(reverse(L)),
    reverse(R).

del_empty(L) ->
    del_empty(L,0).
del_empty([none|L],N) ->
    del_empty(L,N+1);
del_empty(L,N) -> {N,L}.

add_empty(L,W,H) when is_list(L) ->
    Sz = length(L),
    if Sz >= W*H -> L;
       true -> add_empty_1(L,W*H-Sz)
    end.

add_empty(L, Wanted) when length(L) >= Wanted -> L;
add_empty(L, Wanted) ->
    add_empty_1(L, Wanted - length(L)).

add_empty_1(L,N) ->
    L ++ lists:duplicate(N,none).

calc_size(Cols, W, BW, ReduceOne) ->
    ColsW0 = (W-4) div (BW+?BORD),
    ColsW = if ColsW0 < 1 ->
		    1;
	       ReduceOne, ColsW0 > 1 ->
		    ColsW0 - 1;
	       true ->
		    ColsW0
	    end,
    ColsH0 = length(Cols) div ColsW,
    ColsH = if (length(Cols) rem ColsW) == 0 -> ColsH0; true -> ColsH0+1 end,
    {ColsW,ColsH}.

update(Cols,#state{}=State) ->
    Update = fun(St0) ->
		     St = St0#st{pal=Cols},
		     wings_wm:send(geom, {new_state,St}),
		     St
	     end,
    wings_wm:psend(palette, {apply, true, Update}),
    State#state{cols=Cols}.

interpolate([{R1,G1,B1}|_],[Start={R2,G2,B2}|_], N) ->
    R = (R2-R1)/(N+1),	  B = (B2-B1)/(N+1),	G = (G2-G1)/(N+1),
    interpolate(N,R,G,B,Start,[]);
interpolate([],_,_) -> no_start;
interpolate(_,[],_) -> no_end.

interpolate(0,_R,_G,_B,_,Acc) -> Acc;
interpolate(N,R,G,B,{PR,PG,PB},Acc) ->
    Col = {PR-R,PG-G,PB-B},
    interpolate(N-1,R,G,B,Col,[Col|Acc]).

%% drag_and_drop(Ev, What) ->
%%     DropData = {color, color(What)},
%%     wings_wm:drag(Ev, {?BOX_W,?BOX_H}, DropData).

color(none) -> {1.0,1.0,1.0};
color({_,_,_}=C) -> C;
color({R,G,B,_}) -> {R,G,B}.
