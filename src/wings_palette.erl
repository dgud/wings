%%
%%  wings_palette.erl --
%%
%%     Maintains the vertex palette window.
%%
%%  Copyright (c) 2004-2009 Bjorn Gustavsson, Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
-module(wings_palette).

-export([window/1,window/4]).
-export([palette/1]).

-define(NEED_ESDL, 1).
-define(NEED_OPENGL, 1).
-include("wings.hrl").

-import(lists, [foldl/3,reverse/1,keyfind/3]).

-define(BOX_W, 16).
-define(BOX_H, 16).

-define(COLS_W, 8).
-define(COLS_H, 8).

-define(BORD, 2).

-record(pst, {st, sel=none, cols, w=?COLS_W, h=?COLS_H, knob=0}).

window(St) ->
    case wings_wm:is_window(palette) of
	true ->
	    wings_wm:raise(palette),
	    keep;
	false ->
	    {{_,DeskY},{DeskW,_DeskH}} = wings_wm:win_rect(desktop),
	    Pos  = {DeskW-5,DeskY+55},
	    Size = {?COLS_W*?BOX_W+?COLS_W*?BORD+?BORD*2,
		    ?COLS_H*?BOX_H+?COLS_H*?BORD+?BORD*2},
	    window(Pos, Size, [], St),
	    keep
    end.

window(Pos, {W,_}=Size, Ps, St) ->
    Cols = get_all_colors(St),
    {ColsW,ColsH} = calc_size(Cols,W),
    Pst = #pst{st=St, cols=add_empty(Cols,ColsW,ColsH), w=ColsW, h=ColsH},
    Op	= {seq,push,event({current_state,St}, Pst)},
    Props = [{display_lists,geom_display_lists}],
    wings_wm:toplevel(palette, title(), Pos, Size,
		      [{sizeable,?PANE_COLOR},
		       closable, vscroller,
		       {anchor,ne},
		       {properties,Props}|Ps], Op),
    F = fun({color,_}) -> yes;
	   ({material, _}) -> yes;
	   (_) -> no
	end,
    wings_wm:set_prop(palette, drag_filter, F),
    wings_wm:dirty().

title() ->
    ?__(1,"Palette").

palette(#st{pal=[]}) -> [];
palette(#st{pal=Pal0}) ->
    Pal = del_trailing(Pal0),
    case Pal == default_cols() of
	true -> [];
	false -> Pal
    end.

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

get_event(Pst) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> event(Ev, Pst) end}.

event(redraw, Pst) ->
    wings_io:ortho_setup(),
    {W,H} = wings_wm:win_size(),
    wings_io:border(0, 0, W-1, H-1, ?PANE_COLOR),
    draw_objs(Pst),
    keep;

event(resized, Pst=#pst{cols=Cols0,w=CW,h=CH}) ->
    {_,_,W,H} = wings_wm:viewport(),
    {ColsW, _} = calc_size(Cols0, W),
    Visible = H div (?BOX_H+?BORD),
    NewCols = ColsW*Visible,
    OldCols = CW*CH,
    ColsH = if OldCols < NewCols ->
		    Visible;
	       (OldCols rem ColsW) == 0 ->
		    ColsH1 = OldCols div ColsW,
		    ColsH1;
	       true ->
		    {N, _} = del_empty(reverse(Cols0)),
		    Old = OldCols - N,
		    ColsH1 = (Old div ColsW) + 1,
		    if ColsH1 > Visible -> ColsH1;
		       true -> Visible
		    end
	    end,
    update_scroller(0,Visible,ColsH),
    Cols1 = del_trailing(Cols0),
    Cols = add_empty(Cols1,ColsW,ColsH),
    get_event(Pst#pst{knob=0,cols=Cols,w=ColsW,h=ColsH});

event({set_knob_pos, Pos}, Pst = #pst{h=N,knob=Knob}) ->
    case round(Pos*N) of
	Knob -> keep;
	New0  ->
	    New = if New0 < N -> New0; true -> N end,
	    update_scroller(New,N),
	    get_event(Pst#pst{knob=New})
    end;
event(close, _) ->
    delete;
event(got_focus, _) ->
    L = wings_msg:button_format(?__(1,"Assign color to selection")),
    MR = wings_msg:button_format([],
				 ?__(2,"Edit color"),
				 ?__(3,"Show menu")),
    Mods = wings_msg:free_modifier(),
    ModName = wings_msg:mod_name(Mods),
    CL = [ModName,$+,wings_msg:button_format(?__(4,"Clear color"))],
    Msg = wings_msg:join([L,CL,MR]),
    wings_wm:message(Msg),
    keep;

event({current_state,St = #st{pal=StPal}}, Pst=#pst{w=W,h=H}) ->
    case StPal of
	[] ->  %%% Hm... think this through...
	    get_event(Pst#pst{st=St});
	_ ->
	    get_event(Pst#pst{st=St, cols=add_empty(StPal,W,H)})
    end;

event(#mousemotion{state=Bst,x=X,y=Y,mod=Mod}=Ev, #pst{sel=Sel,cols=Cols}=Pst)
  when Bst band ?SDL_BUTTON_LMASK =/= 0 ->
    Delete = Mod band ?CTRL_BITS =/= 0,
    case select(X,Y,Pst) of
	none -> keep;
	Sel -> keep;
	Id when Delete ->
	    {Bef,[_Prev|Rest]} = lists:split(Id, Cols),
	    get_event(update(Bef++[none|Rest],Pst#pst{sel=none}));
	_ when Sel == none ->
	    keep;
	_ ->
	    case lists:nth(Sel+1, Cols) of
		none ->
		    get_event(Pst#pst{sel=none});
		_ ->
		    drag_and_drop(Ev, lists:nth(Sel+1, Cols)),
		    keep
	    end
    end;

event(#mousebutton{button=Butt,x=X,y=Y,mod=Mod,state=?SDL_PRESSED},
      #pst{cols=Cols0}=Pst)
  when Butt =:= 1; Butt =:= 2 ->
    case Mod band wings_msg:free_modifier() =/= 0 of
	false ->
	    get_event(Pst#pst{sel=select(X,Y,Pst)});
	true when Butt =:= 1 ->
	    case select(X,Y,Pst) of
		none -> keep;
		Id ->
		    {Bef,[_Prev|Rest]} = lists:split(Id, Cols0),
		    get_event(update(Bef++[none|Rest],Pst#pst{sel=none}))
	    end;
	true ->
	    keep
    end;

event(#mousebutton{button=1,state=?SDL_RELEASED}, #pst{sel=none}) ->
    keep;

event(#mousebutton{button=2,x=X,y=Y,state=?SDL_RELEASED}, Pst = #pst{sel=Sel}) ->
    case select(X,Y,Pst) of
	Sel ->
	    command({edit,Sel}, Pst);
	_ ->
	    get_event(Pst#pst{sel=none})
    end;

event(#mousebutton{button=1,x=X,y=Y,state=?SDL_RELEASED}, #pst{sel=Sel,cols=Cols,st=St0}=Pst) ->
    Color = lists:nth(Sel+1, Cols),
    case select(X,Y,Pst) of
	none -> keep;
	Sel when Color /= none ->
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
	    wings_wm:send(geom, {new_state,St#st{pal=Cols}}),
	    get_event(Pst#pst{sel=none});
	Sel when Color == none ->
	    command({edit,Sel}, Pst);
	_DropSpot ->
	    get_event(Pst#pst{sel=none})
    end;

event(#mousebutton{x=X0,y=Y0}=Ev, Pst) ->
    Id = select(X0,Y0,Pst),
    case wings_menu:is_popup_event(Ev) of
	{yes,X,Y,_} when is_integer(Id) ->
	    do_menu(Id, X, Y, Pst);
	_ -> keep
    end;

event({action,{palette,Cmd}}, Pst) ->
    command(Cmd, Pst);

event(lost_focus, Pst) ->
    wings_wm:allow_drag(false),
    get_event(Pst#pst{sel=none});

event({new_color,Cols}, Pst = #pst{w=W,h=H0,knob=Knob}) ->
    Sz = length(Cols),
    case Sz == W*H0 of
	true ->
	    get_event(update(Cols,Pst));
	false ->
	    H = if Sz > W*H0 ->
			if (Sz rem W) == 0 -> Sz div W; true -> (Sz div W) + 1 end;
		   true ->
			H0
		end,
	    update_scroller(Knob, H),
	    get_event(update(add_empty(Cols,W,H),Pst#pst{h=H}))
    end;

event({drop,{X,Y},{color,Color}}, #pst{cols=Cols0}=Pst) ->
    case select(X,Y,Pst) of
	none -> keep;
	Id ->
	    {Bef,[_Prev|Rest]} = lists:split(Id, Cols0),
	    Cols = Bef ++ [color(Color)|Rest],
	    get_event(update(Cols, Pst))
    end;
event({drop,{X,Y},{material,Name}}, #pst{cols=Cols0,st=St}=Pst) ->
    case select(X,Y,Pst) of
	none -> keep;
	Id ->
	    {Bef,[_Prev|Rest]} = lists:split(Id, Cols0),
	    Mat = gb_trees:get(list_to_atom(Name), St#st.mat),
	    Color = proplists:get_value(diffuse, proplists:get_value(opengl, Mat)),
	    Cols = Bef ++ [color(Color)|Rest],
	    get_event(update(Cols, Pst))
    end;
event(language_changed, _) ->
    wings_wm:toplevel_title(title()),
    keep;
event(_Ev, _Pst) -> keep.

update_scroller(First,Total) ->
    {_,_,_,H} = wings_wm:viewport(),
    Visible = H div (?BOX_H+?BORD),
    update_scroller(First,Visible,Total).
update_scroller(First,Visible,Total) ->
    Name = wings_wm:this(),
    wings_wm:set_knob(Name, First/Total, Visible/Total).

do_menu(Id,X,Y,#pst{cols=Cols}) ->
    Menu = [{?__(1,"Edit"),{'VALUE',{edit,Id}},?__(2,"Edit color")}],
    Smooth = case lists:nth(Id+1, Cols) of
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
    wings_menu:popup_menu(X,Y,palette,Menu ++ Smooth ++ Rest).

command(clear_all, Pst = #pst{w=W,h=H}) ->
    get_event(update(lists:duplicate(W*H, none),Pst#pst{sel=none}));
command(compact, Pst = #pst{w=W,h=H,cols=Cols0}) ->
    Cols = foldl(fun(none,Acc) -> Acc;
		    (Col, Acc) -> [Col|Acc]
		 end, [], Cols0),
    get_event(update(add_empty(reverse(Cols),W,H),Pst#pst{sel=none}));

command({_,none}, _) -> keep;
command({smooth,Id}, Pst = #pst{cols=Cols0}) ->
    {Bef0,After0} = lists:split(Id, Cols0),
    {BC, Bef1} = del_empty(reverse(Bef0)),
    {AC, Aft1} = del_empty(After0),
    case interpolate(Bef1,Aft1,AC+BC) of
	no_start ->
	    wings_u:message(?__(1,"No start color found.")),
	    keep;
	no_end ->
	    wings_u:message(?__(2,"No end color found.")),
	    keep;
	IntCols ->
	    Cols = reverse(Bef1) ++ IntCols ++ Aft1,
	    get_event(update(Cols,Pst#pst{sel=none}))
    end;
command({edit,Id}, #pst{cols=Cols0}) ->
    {Bef,[Prev|Rest]} = lists:split(Id, Cols0),
    Send = fun(Color) ->
		   Cols = Bef ++ [Color|Rest],
		   wings_wm:send(palette, {new_color,Cols}),
		   ignore
	   end,
    wings_color:choose(color(Prev), Send);

command(scan_all, #pst{st=St, cols=Cols}) ->
    New = scan_colors(St, del_trailing(Cols)),
    wings_wm:send(palette,{new_color,New}),
    keep;

command(export, #pst{cols=Cols}) ->
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
    wings_plugin:call_ui({file,save_dialog,Ps,Fun});

command(import, #pst{cols=Cols0}) ->
    Dir = wings_pref:get_value(current_directory),
    Ps = [{title,?__(7,"Import")},{directory,Dir},
	  {ext,".wpal"},{ext_desc,?__(9,"Wings Palette")}],
    Fun = fun(Name) ->
		  case file:consult(Name) of
		      {ok,Content} ->
			  case keyfind(palette,1,Content) of
			      {palette, Pal} when is_list(Pal) ->
				  Cols = del_trailing(Cols0) ++ Pal,
				  wings_wm:send(palette, {new_color,Cols}),
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

write_file(Name, Cols) ->
    case file:open(Name, [write]) of
	{ok,F} ->
	    io:format(F, "%% Wings Palette~n", []),
	    io:format(F, "{palette, ~p}.~n", [del_trailing(Cols)]),
	    file:close(F);
	Error -> Error
    end.

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
       true -> add_empty(L,W*H-Sz)
    end.
add_empty(L,N) ->
    L ++ lists:duplicate(N,none).

calc_size(Cols, W) ->
    ColsW0 = W div (?BOX_W+?BORD),
    ColsW = if ColsW0 < 1 -> 1; true -> ColsW0 end,
    ColsH0 = length(Cols) div ColsW,
    ColsH = if (length(Cols) rem ColsW) == 0 -> ColsH0; true -> ColsH0+1 end,
    {ColsW,ColsH}.

update(Cols,Pst=#pst{st=St0}) ->
    St = St0#st{pal=Cols},
    wings_wm:send(geom, {new_state,St}),
    Pst#pst{cols=Cols, st=St}.

interpolate([{R1,G1,B1}|_],[Start={R2,G2,B2}|_], N) ->
    R = (R2-R1)/(N+1),	  B = (B2-B1)/(N+1),	G = (G2-G1)/(N+1),
    interpolate(N,R,G,B,Start,[]);
interpolate([],_,_) -> no_start;
interpolate(_,[],_) -> no_end.

interpolate(0,_R,_G,_B,_,Acc) -> Acc;
interpolate(N,R,G,B,{PR,PG,PB},Acc) ->
    Col = {PR-R,PG-G,PB-B},
    interpolate(N-1,R,G,B,Col,[Col|Acc]).

drag_and_drop(Ev, What) ->
    DropData = {color, color(What)},
    wings_wm:drag(Ev, {?BOX_W,?BOX_H}, DropData).

draw_objs(#pst{cols=Cols0, w=W, h=_H, knob=Knob}) ->
    {_Bef,Cols} = lists:split(Knob*W, Cols0),
    draw_objs(0, ?BORD, ?BORD, W, Cols).
draw_objs(_,_,_,_,[]) ->    ok;
draw_objs(N,X,Y,W,[Color|Cols]) when N < W ->
    case Color of
	none ->
	    wings_io:border(X, Y, ?BOX_W, ?BOX_H, ?PANE_COLOR);
	_ ->
	    wings_io:border(X, Y, ?BOX_W, ?BOX_H, Color)
    end,
    draw_objs(N+1, X+?BOX_W+?BORD, Y, W, Cols);
draw_objs(_,_,Y,W,Cols) ->
    draw_objs(0, ?BORD, Y+?BOX_H+?BORD,W,Cols).

color(none) -> {1.0,1.0,1.0};
color({_,_,_}=C) -> C;
color({R,G,B,_}) -> {R,G,B}.

select(X,Y,#pst{w=ColsW,h=ColsH, knob=Knob}) ->
    Col = X div (?BORD+?BOX_W),
    Row = Knob + (Y div (?BORD+?BOX_H)),
    Id =  (Row*ColsW+Col),
    if
	X > ColsW *(?BORD+?BOX_W) -> none;
	Y > ColsH *(?BORD+?BOX_H) -> none;
	Id < 0 -> none;
	Id >= (ColsW*ColsH) -> none;
	true -> Id
    end.

scan_colors(#st{mat=Mtab, selmode=Mode}=St, Cols0) ->
    Cols1 = scan_materials(gb_trees:values(Mtab), Cols0),

    Cols  = wings_sel:fold(fun(Items, We, Cols) ->
				   scan_color(Items, Mode, We, Cols)
			   end, ordsets:from_list(Cols1), St),
    lists:usort(Cols).

scan_color(_, body, We, Acc) ->
    Cols = wings_va:all(color, We),
    ordsets:union(Cols, Acc);
scan_color(Sel, vertex, We, Acc0) ->
    gb_sets:fold(
      fun(V, Acc1) ->
	      wings_vertex:fold(fun(_, Face, _, Acc) ->
					Attr = wings_va:vtx_attrs(V, Face, We),
					add_cols([Attr], Acc)
				end, Acc1, V, We)
      end, Acc0, Sel);
scan_color(Sel, edge, We = #we{es=Etab}, Acc0) ->
    gb_sets:fold(
      fun(Edge, Acc) ->
	      #edge{lf=LF,rf=RF,ve=Ve,vs=Vs} = array:get(Edge, Etab),
	      A = wings_va:vtx_attrs(Vs, LF, We),
	      B = wings_va:vtx_attrs(Vs, RF, We),
	      C = wings_va:vtx_attrs(Ve, LF, We),
	      D = wings_va:vtx_attrs(Ve, RF, We),
	      add_cols([A,B,C,D], Acc)
      end, Acc0, Sel);
scan_color(Sel, face, We, Acc0) ->
    gb_sets:fold(
      fun(Face, Acc) ->
	      Attrs = wings_va:face_attr(color, Face, We),
	      add_cols(Attrs, Acc)
      end, Acc0, Sel).

scan_materials([Mat|Ms], Cols) ->
    Opengl = proplists:get_value(opengl, Mat),
    Diff   = proplists:get_value(diffuse, Opengl),
    Amb    = proplists:get_value(ambient, Opengl),
    Spec   = proplists:get_value(specular, Opengl),
    Emis   = proplists:get_value(emission, Opengl),
    scan_materials(Ms, [color(Diff),color(Amb),color(Spec),
			color(Emis)|Cols]);
scan_materials([], Cols) -> Cols.

add_cols([none|R], Acc) ->
    add_cols(R,Acc);
add_cols([Col|R], Acc) when tuple_size(Col) =:= 3 ->
    add_cols(R, [Col|Acc]);
add_cols([Attr|R], Acc) ->
    add_cols(R, [wings_va:attr(color, Attr)|Acc]);
add_cols([], Acc) -> Acc.
