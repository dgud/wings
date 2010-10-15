%%
%%  wings_toolbar.erl --
%%
%%     Toolbar for geometry and AutoUV windows.
%%
%%  Copyright (c) 2004-2007 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_toolbar).

-export([create/3]).
-export([button_help_2/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foreach/2]).

%%%
%%% The toolbar window with buttons.
%%%

-define(BUTTON_WIDTH, 34).
-define(BUTTON_HEIGHT, 28).

-record(but,
	{mode=face :: sel_mode(),		%Selection mode.
	 sh=true :: boolean(),			%Smart highlighting (true|false).
	 buttons,				%Buttons to show.
	 all_buttons,				%All buttons.
	 restr=none				%Restriction (none|[Mode]).
	}).

create({toolbar,Client}=Name, Pos, W) ->
    ButtonH = ?BUTTON_HEIGHT+6,
    wings_wm:new(Name, Pos, {W,ButtonH}, init_button()),
    wings_wm:set_prop(Name, display_lists, wings_wm:get_prop(Client, display_lists)).

init_button() ->
    {seq,push,get_button_event(#but{})}.

get_button_event(But) ->
    {replace,fun(Ev) -> button_event(Ev, But) end}.

button_event({window_updated,_}, But) ->
    get_button_event(button_resized(But));
button_event(redraw, #but{buttons=undefined}=But0) ->
    But = button_resized(But0),
    button_redraw(But),
    get_button_event(But);
button_event(redraw, But) ->
    button_redraw(But),
    keep;
button_event(#mousebutton{button=B,x=X,state=?SDL_PRESSED}, But) when B =< 3 ->
    button_was_hit(X, But),
    keep;
button_event(#mousebutton{button=B,x=X,state=?SDL_RELEASED}, But) when B =< 3 ->
    button_help(X, But),
    keep;
button_event(#mousemotion{x=X}, But) ->
    button_help(X, But),
    keep;
button_event({action,_}=Action, _) ->
    {toolbar,Client} = wings_wm:this(),
    wings_wm:send(Client, Action);
button_event({current_state,#st{selmode=Mode,sh=Sh}}, #but{mode=Mode,sh=Sh}) ->
    keep;
button_event({current_state,#st{selmode=Mode,sh=Sh}}, But) ->
    wings_wm:dirty(),
    get_button_event(But#but{mode=Mode,sh=Sh});
button_event({mode_restriction,Restr}, #but{restr=Restr}) ->
    keep;
button_event({mode_restriction,Restr}, #but{all_buttons=undefined}=But) ->
    get_button_event(But#but{restr=Restr});
button_event({mode_restriction,Restr}, #but{all_buttons=AllButtons}=But) ->
    Buttons = button_restrict(AllButtons, Restr),
    wings_wm:dirty(),
    get_button_event(But#but{buttons=Buttons,restr=Restr});
button_event(#keyboard{}=Ev, _) ->
    {toolbar,Client} = wings_wm:this(),
    wings_wm:send(Client, Ev);
button_event(_, _) -> keep.

button_resized(#but{restr=Restr}=But) ->
    {toolbar,Client} = Self = wings_wm:this(),
    {{X,Y},{W,_}} = wings_wm:win_rect(Client),
    {_,H} = wings_wm:win_size(),
    wings_wm:update_window(Self, [{x,X},{y,Y-H},{w,W}]),
    AllButtons = buttons_place(W),
    Buttons = button_restrict(AllButtons, Restr),
    But#but{buttons=Buttons,all_buttons=AllButtons}.

button_redraw(#but{mode=Mode,buttons=Buttons,sh=Sh0}) ->
    Sh = button_sh_filter(Mode, Sh0),
    wings_io:ortho_setup(),
    {W,H} = wings_wm:win_size(),
    wings_io:border(0, 0, W-1, H-1, ?PANE_COLOR),
    wings_io:draw_icons(
      fun() ->
	      foreach(fun({X,Name}) ->
			      wings_io:draw_icon(X, 3, button_value(Name, Mode, Sh))
		      end, Buttons)
      end),
    button_redraw_sh(Sh, Buttons).

button_redraw_sh(false, _) -> ok;
button_redraw_sh(true, Buttons) ->
    Pos = [X || {X,M} <- Buttons, button_sh_filter(M, true)],
    case Pos of
	[] -> ok;
	[Left|_] ->
	    gl:pushAttrib(?GL_POLYGON_BIT bor ?GL_LINE_BIT),
	    gl:polygonMode(?GL_FRONT_AND_BACK, ?GL_LINE),
	    Right = lists:last(Pos),
	    gl:color3f(1, 1, 1),
	    gl:recti(Left-1, 3, Right+?BUTTON_WIDTH, 3+?BUTTON_HEIGHT),
	    gl:popAttrib()
    end.

button_sh_filter(_, false) -> false;
button_sh_filter(vertex, true) ->
    wings_pref:get_value(vertex_hilite);
button_sh_filter(edge, true) ->
    wings_pref:get_value(edge_hilite);
button_sh_filter(face, true) ->
    wings_pref:get_value(face_hilite);
button_sh_filter(_, _) -> false.

buttons_place(W) when W < 140 -> [];
buttons_place(W) when W < 325 ->
    Mid = (W - ?BUTTON_WIDTH) div 2,
    button_place_0(Mid);
buttons_place(W) when W < 485 ->
    Mid = (W - ?BUTTON_WIDTH) div 2,
    Lmarg = 5,
    Rmarg = 5,
    case wings_pref:get_value(extended_toolbar) of
      true ->
        button_place_0(Mid);
      false ->
        button_place_0(W,Mid,Lmarg,Rmarg)
    end;
buttons_place(W) ->
    Mid = (W - ?BUTTON_WIDTH) div 2,
    Lmarg = 5,
    Rmarg = 5,
    case wings_pref:get_value(extended_toolbar) of
      true ->
        [{Lmarg,open},
         {Lmarg+?BUTTON_WIDTH,save},
         {Lmarg+2*?BUTTON_WIDTH,undo},
         {Lmarg+3*?BUTTON_WIDTH,redo},
         {Mid-trunc(1.5*?BUTTON_WIDTH),vertex},
         {Mid-trunc(0.5*?BUTTON_WIDTH),edge},
         {Mid+trunc(0.5*?BUTTON_WIDTH),face},
         {Mid+trunc(1.5*?BUTTON_WIDTH),body},
         {W-5*?BUTTON_WIDTH-Rmarg,pref},
         {W-4*?BUTTON_WIDTH-Rmarg,smooth},
         {W-3*?BUTTON_WIDTH-Rmarg,perspective},
         {W-2*?BUTTON_WIDTH-Rmarg,groundplane},
         {W-?BUTTON_WIDTH-Rmarg,axes}];
      false ->
        button_place_0(W,Mid,Lmarg,Rmarg)
    end.

button_place_0(Mid) ->
    [{Mid-trunc(1.5*?BUTTON_WIDTH),vertex},
     {Mid-trunc(0.5*?BUTTON_WIDTH),edge},
     {Mid+trunc(0.5*?BUTTON_WIDTH),face},
     {Mid+trunc(1.5*?BUTTON_WIDTH),body}].

button_place_0(W,Mid,Lmarg,Rmarg) ->
    [{Lmarg,smooth},
     {Lmarg+?BUTTON_WIDTH,perspective},
     {Mid-trunc(1.5*?BUTTON_WIDTH),vertex},
     {Mid-trunc(0.5*?BUTTON_WIDTH),edge},
     {Mid+trunc(0.5*?BUTTON_WIDTH),face},
     {Mid+trunc(1.5*?BUTTON_WIDTH),body},
     {W-2*?BUTTON_WIDTH-Rmarg,groundplane},
     {W-?BUTTON_WIDTH-Rmarg,axes}].

button_value(groundplane=Name, _, _) ->
    button_value_1(Name, show_groundplane, true);
button_value(axes=Name, _, _) ->
    button_value_1(Name, show_axes, true);
button_value(smooth=Name, _, _) ->
    button_value_1(Name, workmode, false);
button_value(perspective=Name, _, _) ->
    button_value_1(Name, orthogonal_view, true);
button_value(Mode, Mode, false) -> {Mode,down};
button_value(Name, _, _) -> {Name,up}.

button_value_1(Name, Key, Val) ->
    {toolbar,Client} = wings_wm:this(),
    case wings_wm:get_prop(Client, Key) of
	Val -> {Name,down};
	_ -> {Name,up}
    end.

button_was_hit(X, #but{buttons=Buttons}) ->
    button_was_hit_1(X, Buttons).

button_was_hit_1(X, [{Pos,Name}|_]) when Pos =< X, X < Pos+?BUTTON_WIDTH ->
    Action = case Name of
		 groundplane -> {view,show_groundplane};
		 axes -> {view,show_axes};
		 flatshade -> {view,flatshade};
		 smooth -> {view,workmode};
		 perspective -> {view,orthogonal_view};
		 undo -> {edit,undo};
		 redo -> {edit,redo};
		 open -> {file,open};
		 save -> {file,save};
		 pref -> {edit,{preferences,prefs}};
		 Other -> {select,Other}
	     end,
    {toolbar,Client} = wings_wm:this(),
    wings_wm:send(Client, {action,Action});
button_was_hit_1(X, [_|Is]) ->
    button_was_hit_1(X, Is);
button_was_hit_1(_X, []) ->
    wings_wm:send(geom, {action,{select,deselect}}),
    try 
	%% Try to load any new code, but ignore any errors.
	case user_default:lm() of
	    [] -> ok;
	    Mods ->
		{_,Ms} = lists:unzip(Mods),
		io:fwrite("Reloaded: ~p\n", [Ms])
	end
    catch
	_:_ ->
	    %% If the beam files have been stripped (as in the Wings distribution)
	    %% user_default:lm/0 will crash.
	    ok
    end.

button_help(X, #but{mode=Mode,sh=false,buttons=Buttons}) ->
    wings_wm:message(button_help_1(X, Buttons, Mode));
button_help(X, #but{sh=true,buttons=Buttons}) ->
    %% Smart highlighting is on. We don't really have any mode.
    %% Make sure that the help texts reflects that.
    wings_wm:message(button_help_1(X, Buttons, undecided)).

button_help_1(X, [{Pos,Name}|_], Mode) when Pos =< X, X < Pos+?BUTTON_WIDTH ->
    button_help_2(Name, Mode);
button_help_1(X, [_|Is], Mode) ->
    button_help_1(X, Is, Mode);
button_help_1(_, [], _) ->
    ?__(1,"Deselect").

button_help_2(vertex, vertex) -> ?__(1,"Select adjacent vertices");
button_help_2(vertex, _) ->  ?__(2,"Change to vertex selection mode");
button_help_2(edge, edge) ->  ?__(3,"Select adjcacent edges");
button_help_2(edge, _) ->  ?__(4,"Change to edge selection mode");
button_help_2(face, face) ->  ?__(5,"Select adjacent faces");
button_help_2(face, _) ->  ?__(6,"Change to face selection mode");
button_help_2(body, body) -> "";
button_help_2(body, _) ->  ?__(7,"Change to body selection mode");
button_help_2(Button, _) -> button_help_3(Button).

button_help_3(groundplane) ->
    [choose(show_groundplane, true, hide(), show()),
     " "|?STR(messages,groundplane,"ground plane")];
button_help_3(axes) ->
    [choose(show_axes, true, hide(), show()),
     " "|?STR(messages,axes,"axes")];
button_help_3(perspective) ->
    [?STR(messages,change_to,"Change to")," ",
     choose(orthogonal_view, false,
	    ?STR(messages,orthogonal,"orthogonal view"),
	    ?STR(messages,perspective,"perspective view"))];
button_help_3(smooth) ->
    [?STR(messages,show_objects,"Show objects with")," ",
     choose(workmode, true,
	    ?STR(messages,smooth,"smooth shading"),
	    ?STR(messages,flat,"flat shading"))];

button_help_3(undo) -> ?__(1,"Undo the last command");
button_help_3(redo) -> ?__(2,"Redo the last command that was undone");
button_help_3(open) -> ?__(3,"Open a previously saved scene");
button_help_3(save) -> ?__(4,"Save the current scene");
button_help_3(pref) -> ?__(5,"Edit the preferences for Wings").

button_restrict(Buttons, none) -> Buttons;
button_restrict(Buttons0, Restr) ->
    Buttons1 = sofs:from_external(Buttons0, [{atom,atom}]),
    Buttons = sofs:restriction(2, Buttons1, sofs:set(Restr)),
    sofs:to_external(Buttons).

choose(Key, Val, First, Second) ->
    {toolbar,Client} = wings_wm:this(),
    case wings_wm:get_prop(Client, Key) of
	Val -> First;
	_ -> Second
    end.

hide() -> ?STR(messages,hide,"Hide").
show() -> ?STR(messages,show,"Show").

