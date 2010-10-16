%%
%%  wings_help.erl --
%%
%%     This module implements the Help menu.
%%
%%  Copyright (c) 2001-2009 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_help).
-export([menu/1,command/2]).
-export([cmd/1,help_window/2,no_more_basic_menus/0,
	 not_possible_to_save_prefs/0]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(erlang, [max/2]).
-import(lists, [foldl/3]).

menu(_) ->
    L = [getting_started,
	 one_or_two,
	 international,
	 separator,
	 hotkeys,
	 defining_hotkeys,
	 separator,
	 lights,
	 separator,
	 default_commands,
	 separator,
	 tweak_help,
	 separator,
	 performance_tips,
	 separator,
	 opengl_info,
	 separator,
	 about],
    [if
	 I == separator -> I;
	 true -> {header(I),I}
     end || I <- L].

header(getting_started) ->
    ?__(1,"Getting Started");
header(one_or_two) ->
    ?__(2,"Using a Mouse With One or Two Buttons");
header(international) ->
    ?__(3,"French and German Keyboards");
header(hotkeys) ->
    ?__(4,"Defined Hotkeys");
header(defining_hotkeys) ->
    ?__(5,"How To Define Hotkeys");
header(lights) ->
    ?__(6,"Light Basics");
header(default_commands) ->
    ?__(8,"Default Commands");
header(performance_tips) ->
    ?__(9,"Performance Tips");
header(opengl_info) ->
    ?__(10,"OpenGL Info");
header(about) ->
    ?__(11,"About Wings 3D");
header(tweak_help) ->
    ?__(12,"Tweak").

command(tweak_help, St) ->
    wings_tweak:command(show_help, St);
command(Item, _St) ->
    command_1(Item, header(Item)).

command_1(getting_started, Header) ->
    getting_started(Header);
command_1(one_or_two, Header) ->
    one_or_two(Header);
command_1(international, Header) ->
    international(Header);
command_1(defining_hotkeys, Header) ->
    def_hotkeys(Header);
command_1(hotkeys, Header) ->
    hotkeys(Header);
command_1(default_commands, Header) ->
    def_commands(Header);
command_1(lights, Header) ->
    lights(Header);
command_1(performance_tips, Header) ->
    performance_tips(Header);
command_1(opengl_info, Header) ->
    opengl_info(Header);
command_1(about, Header) ->
    about(Header).


no_more_basic_menus() ->
    Qs = {vframe,
	  [{label,"You are seeing this message because the Advanced pop-up menus option was turned off in your preferences."},
	   panel,
	   {label,"From now on, Wings will only support Advanced pop-up menus."},
	   panel,
	   {label,"The major difference compared to the Basic pop-up menus, which you have been using, is that"},
	   {label,"all three mouse buttons can be used to execute a command."},
	   panel,
	   {label,"Usually, the most common variation of a command is accessed by clicking the left mouse button,"},
	   {label,"while the middle and right mouse buttons provide more advanced options for the command."},
	   panel,
	   {label,"Details about commands activated by each mouse button are displayed in the Information Line"},
	   {label,"across the bottom of your screen when you hover over any menu item."},
	   panel,
	   {hframe,[{"Never show this message again",false,
		     [{key,no_basic_menu_info}]},
		    {button,ok,[ok]}]}]},
    wings_ask:dialog("", Qs, fun([{K,V}]) ->
				     wings_pref:set_value(K, V),
				     ignore
			     end).

not_possible_to_save_prefs() ->
    H = [{label,"The Wings3D.exe program was not able to locate the folder "
	  "for applications settings."},
	 panel,
	 {label,"This is not supposed to happen. "
	  "You should report this bug."},
	 panel,
	 {hframe,[{button,ok,[ok]}]}],
    Qs = {vframe,H},
    wings_ask:dialog("", Qs, fun(_) -> ignore end).

getting_started(Head) ->
    B = "(",
    E = ")",
    H = [?__(1,
	      "When learning Wings, keep an eye on the information line at the bottom of the screen."
	      " Generally it shows what the mouse buttons will do at any given moment."),

	 ?__(2,
	      "The actions for the mouse buttons are given first, labeled "),

	 wings_msg:button_format(B++?__(left_def,
					  "left mouse button")++E),
	 wings_msg:button_format([],
				  B++?__(middle_def,
					  "middle button or scroll wheel")++E),
	 wings_msg:button_format([], [],
				  B++?__(right_def,
					  "right button")++E),

	 ?__(two_a,
	      "followed by any hotkey actions."),

	 use_one_or_two(),
	 ?__(7,
	      "Generally, L (left mouse button) is used for selecting and accepting, M (middle mouse button) for operating the camera, and R (right mouse button) to access the context-sensitive pop-up menus."),
	 ?__(8, "Note that different mouse buttons can be used in pop-up menus to choose different variations of a command. Generally, the most common way to execute the command is found on the left mouse buttons, while the more advanced commands are found on the middle and right mouse buttons.")
	],
    help_window(Head, H).

one_or_two(Head) ->
    Nendo = [{ul,wings_s:camera_mode(nendo)}],
    Help = [use_one_or_two(),
	    ?__(1,"Note that only the ")++
	    Nendo++
	    ?__(2," and ")++
	    [{ul,wings_s:camera_mode(blender)}]++
	    ?__(3,
		 " modes can be used with a two-button mouse."
		 " Only the ")++
	    Nendo++
	    ?__(4,
		 " mode can be used with an one-button mouse.")
	   ],
    help_window(Head, Help).

use_one_or_two() ->
    ?__(1,
	 "To use mice with only one or two buttons, "
	 "you must inform Wings how many buttons "
	 "your mouse has in the ")++
	edit_prefs()++
	?__(2," dialog.").

international(Head) ->
    Help = [?__(1,"Unfortunately, on French and German keyboards (and possibly others), the Undo/Redo commands will not be bound to the [Z] key. (That might be changed in a future release of Wings.)"),
	    ?__(2,"On French keyboards, the Undo/Redo commands are found on the [W] key ([Ctrl]+[W], [Ctrl]+[Alt]+[W] and so on)."),
	    ?__(3,"On German keyboards, the Undo/Redo commands are found on the [Y] key ([Ctrl]+[Y], [Ctrl]+[Alt]+[Y] and so on).")],
    help_window(Head, Help).

def_commands(Head) ->
    Ctrl = wings_s:key(ctrl)++ "+",
    ShiftCtrl = wings_s:key(shift)++ "+" ++ Ctrl,
    Help = [?__(1,"In the ")++
	    edit_prefs()++
	    ?__(4,
		 " dialog, you can turn on \"Default Commands\"."),
	    ?__(5,"Two default commands can be defined."
		 " To save the previous command that was executed, use one of:"),
	    "  " ++ ShiftCtrl ++ wings_s:lmb(),
	    "  " ++ ShiftCtrl ++ wings_s:mmb(),
	    ?__(8,"To use a command that has been defined this way, use one of:"),
	    "  " ++ Ctrl ++ wings_s:lmb(),
	    "  " ++ Ctrl ++ wings_s:mmb(),
	    ?__(11,"Note: When using the ") ++
	    [{ul,wings_s:camera_mode(tds)}] ++
	    ?__(13," or ") ++
	    [{ul,wings_s:camera_mode(blender)}] ++
	    ?__(15,
		 " camera modes, the second default command cannot be used.")],
    help_window(Head, Help).

performance_tips(Head) ->
    B = [bullet]++" ",
    H = [?__(1,"The performance of Wings is dependent on many different things, such as"),
	 B++?__(2,"the speed of the CPU"),
	 B++?__(3,"type and size of the CPU cache"),
	 B++?__(4,"amount and speed of memory"),
	 B++?__(5,"type of graphics card"),
	 B++?__(6,"amount of video memory"),
	 B++?__(7,"the phase of the moon"),

	 ?__(8,"Therefore, it is difficult to give any firm advice on how to improve Wings performance. The following tips MAY improve performance:"),

	 B++?__(31,"Use fewer Undo levels. Setting to 4 or less (default 32) will let you work on models with almost 8 times the detail. In this case, using a shorter autosave period (default 10mins) may be advisable."),

	 B++?__(9,"Try different number of colors and different screen resolutions. Especially if the graphics card doesn't have much memory, many colors and/or high resolution may drastically reduce performance. Using a smaller Wings window (not maximized) may also help."),

	 B++?__(10,"Close unnecessary windows inside Wings."),

	 B++?__(11,"Make sure that Geometry windows don't overlap."),

	 B++?__(12,"Use as few (active) lights as possible. More lights means less speed on most grahics cards."),

	 B++?__(13,"If possible, use the ")
	  ++cmd([?__(14,"Tools"),
		 ?__(15,"Virtual Mirror")])
	  ++?__(16," command."),

	 B++?__(17,"Hide models that you don't work on for the moment."),

	 B++?__(18,"Use the ")
	  ++cmd([?__(19,"View"),
	         ?__(20,"Show Colors")])
	  ++?__(21," command to turn off vertex color display if your model has vertex colors."),

	 B++?__(22,"Use the ")
	  ++cmd([?__(23,"View"),
	         ?__(24,"Show Textures")])
	  ++?__(25," command to turn off textures while modeling."),

	 B++?__(26,"Work in wireframe mode."),

	 B++?__(27,"Some graphics cards display edges slowly. Turn off edge display using the ")
	  ++cmd([?__(28,"View"),
	         ?__(29,"Show Edges")])
	  ++?__(30," command.")
	],
    help_window(Head, H).

hotkeys(Head) ->
    Help = wings_hotkey:listing(),
    help_window(Head, Help).

def_hotkeys(Head) ->
    Help = [?__(1,"Any command that appears in a menu, can be assigned a keyboard short-cut (hotkey)."),
	    ?__(4,"To assign a hotkey to a command:"),
	    ?__(5,"1. Open the menu containing the command and highlight the command."),
	    ?__(6,"2. Press the [Insert] or [/] key."),
	    ?__(10,"3. If the command does different things depending on "
		"which mouse button is pressed there will be a message in "
		"the information line asking you to press a mouse button "
		"to choose which variant of the command to assign to "
		"the hotkey. (Note: The mouse cursor must remain inside "
		"the menu when you click.)"),
	    ?__(8,"4. The information line asks you to press the key that the command "
		"should be assigned to."),

	    ?__(9,"To delete a hotkey, similarly highlight the command in a menu,"
		" and press the [Del] or [\\] key. A dialog box listing all keys "
		"bound to the command will appear. "
		"Check all hotkeys you want to delete.")],
    help_window(Head, Help).

lights(Head) ->
    Help = [?__(1,"1. Create lights using the Light command in the primitives menu (R-click when there is no selection)."),
    	    ?__(2,"2. Select a light by L-clicking on it. When any light is selected, a special Light menu will pop up when you R-click."),
	    ?__(3,"3. To tell Wings to actually use the lights you have created, use the ")
	    ++cmd([?__(4,"View"),
	 	   ?__(5,"Scene Lights")
		  ])
	    ++?__(6," command.")],
    help_window(Head, Help).

opengl_info(Head) ->
    gl:getError(),			%Clear any previous error.
    [{_,VerTuple}] = ets:lookup(wings_gl_ext, version),
    BackendVersion = "2D-API: " ++ wings_io:version_info(),
    Help = [
	    ?__(1,"Vendor: ") ++ gl:getString(?GL_VENDOR) ++ "\n" ++
	    ?__(2,"Renderer: ") ++ gl:getString(?GL_RENDERER) ++ "\n" ++
	    ?__(3,"Version: ") ++ gl:getString(?GL_VERSION) ++ "\n" ++
	    ?__(4,"Version tuple: ") ++ lists:flatten(io_lib:format("~p\n", [VerTuple])) ++
	    BackendVersion,

	    get_info([{?__(5,"Red bits"),?GL_RED_BITS},
		      {?__(6,"Green bits"),?GL_GREEN_BITS},
		      {?__(7,"Blue bits"),?GL_BLUE_BITS},
		      {?__(8,"Alpha bits"),?GL_ALPHA_BITS},
		      {?__(9,"Depth bits"),?GL_DEPTH_BITS},
		      {?__(10,"Stencil bits"),?GL_STENCIL_BITS},
		      {?__(11,"Accum. red bits"),?GL_ACCUM_RED_BITS},
		      {?__(12,"Accum. green bits"),?GL_ACCUM_GREEN_BITS},
		      {?__(13,"Accum. blue bits"),?GL_ACCUM_BLUE_BITS},
		      {?__(14,"Accum. alpha bits"),?GL_ACCUM_ALPHA_BITS},
		      {?__(15,"Max number of lights"),?GL_MAX_LIGHTS},
		      {?__(16,"Max clip planes"),?GL_MAX_CLIP_PLANES},
		      {?__(17,"Max modelview stack depth"),?GL_MAX_MODELVIEW_STACK_DEPTH},
		      {?__(18,"Max projection stack depth"),?GL_MAX_PROJECTION_STACK_DEPTH},
		      {?__(19,"Max texture stack depth"),?GL_MAX_TEXTURE_STACK_DEPTH},
		      {?__(20,"Subpixel bits"),?GL_SUBPIXEL_BITS},
		      {?__(21,"Max 3D texture size"),?GL_MAX_3D_TEXTURE_SIZE},
		      {?__(22,"Max texture size"),?GL_MAX_TEXTURE_SIZE},
		      {?__(23,"Max pixel map table"),?GL_MAX_PIXEL_MAP_TABLE},
		      {?__(24,"Max name stack depth"),?GL_MAX_NAME_STACK_DEPTH},
		      {?__(25,"Max display-list call nesting"),?GL_MAX_LIST_NESTING},
		      {?__(26,"Max evaluator polynomial order"),?GL_MAX_EVAL_ORDER},
		      {?__(27,"Max viewport dimensions"),?GL_MAX_VIEWPORT_DIMS},
		      {?__(28,"Max depth of attribute stack"),?GL_MAX_ATTRIB_STACK_DEPTH},
		      {?__(29,"Max depth of client attribute stack"),
		       ?GL_MAX_CLIENT_ATTRIB_STACK_DEPTH},
		      {?__(30,"Number of auxiliary buffers"),?GL_AUX_BUFFERS},
		      {?__(31,"Color buffers store RGBA"),?GL_RGBA_MODE},
		      {?__(32,"Color buffers store indices"),?GL_INDEX_MODE},
		      {?__(33,"Double buffering"),?GL_DOUBLEBUFFER},
		      {?__(34,"Stereo buffers"),?GL_STEREO},
		      {?__(45,"Sample buffers"),?GL_SAMPLE_BUFFERS},
		      {?__(35,"Range of aliased point sizes"),?GL_ALIASED_POINT_SIZE_RANGE},
		      {?__(36,"Range of antialised point sizes"),?GL_SMOOTH_POINT_SIZE_RANGE},
		      {?__(37,"Range of aliased line widths"),?GL_ALIASED_LINE_WIDTH_RANGE},
		      {?__(38,"Range of antialised line widths"),?GL_SMOOTH_LINE_WIDTH_RANGE},
		      {?__(39,"Recommended max number of indices for drawRangeElement()"),
		       ?GL_MAX_ELEMENTS_INDICES},
		      {?__(40,"Recommended max number of vertices for drawRangeElement()"),
		       ?GL_MAX_ELEMENTS_VERTICES}]),
	    get_info([{?__(41,"Max number of texturing units"),?GL_MAX_TEXTURE_UNITS},
		      {?__(42,"Number of compression formats"),
		       ?GL_NUM_COMPRESSED_TEXTURE_FORMATS},
		      {?__(43,"Max number of vertex units"),?GL_MAX_VERTEX_UNITS_ARB}]),
		?__(44,"OpenGL Extensions"),extensions()],
    help_window(Head, Help).

get_info([{Label,Attr}|T]) ->
    Val = gl:getIntegerv(Attr),
    ValStr = case {gl:getError(),Val} of
		 {0,List} ->
		     get_info_1(Attr, List);
		 _ -> "---"
	     end,
    Label ++ ": " ++ ValStr ++ "\n" ++ get_info(T);
get_info([]) -> [].

get_info_1(_, [A]) -> integer_to_list(A);
get_info_1(Enum, [A,B|_]) ->
    case has_one_elem(Enum) of
	false -> integer_to_list(A) ++ ", " ++ integer_to_list(B);
	true -> integer_to_list(A)
    end.

has_one_elem(?GL_MAX_ELEMENTS_VERTICES) -> true;
has_one_elem(?GL_MAX_ELEMENTS_INDICES) -> true;
has_one_elem(?GL_NUM_COMPRESSED_TEXTURE_FORMATS) -> true;
has_one_elem(?GL_MAX_VERTEX_UNITS_ARB) -> true;
has_one_elem(?GL_MAX_3D_TEXTURE_SIZE) -> true;
has_one_elem(?GL_SAMPLE_BUFFERS) -> true;
has_one_elem(_) -> false.

extensions() ->
    extensions(lists:sort(string:tokens(gl:getString(?GL_EXTENSIONS), " "))).

extensions([H|T]) -> H ++ "\n" ++ extensions(T);
extensions([]) -> [].

%%%
%%% Scrollable help window.
%%%

-record(ts,
	{lines,
	 first,
	 tw,
	 th,
	 wh
	 }).

help_window(Title, []) ->
    help_window(Title, [?__(1,"No help text")]);
help_window(Title, Text) ->
    help_window(help, Title, Text).

help_window(Name, Title, Text0) ->
    wings_wm:delete(Name),
    Text = [if is_binary(Line) -> binary_to_list(Line);
	       true -> Line end || Line <- Text0],
    {Rows,Lines} = wings_text:break_lines(Text, 60),
    {W,H} = wings_wm:top_size(),
    MaxH = trunc(H*0.75),
    Cw = wings_text:width(),
    Lh = wings_text:height()+2,
    Xs = 64*Cw,
    Ys = case Rows*Lh of
	     Ys0 when Ys0 > MaxH -> MaxH;
	     Ys0 -> Ys0
	 end,
    X = trunc((W-Xs) / 2),
    Y = trunc((H-Ys) / 2),
    Ts = #ts{lines=Lines,first=0,tw=Xs,th=Ys0,wh=Ys},
    Op = {seq,push,get_help_event(Ts)},
    Size = {Xs+Cw,Ys+Lh},
    wings_wm:toplevel(Name, Title, {X,Y,highest}, Size,
		      [closable,vscroller], Op),
    wings_wm:dirty().

get_help_event(Ts) ->
    {replace,fun(Ev) ->
		     handle_help_event(Ev, Ts)
	     end}.

handle_help_event(redraw, DrawData) ->
    redraw(DrawData),
    keep;
handle_help_event(close, _) -> delete;
handle_help_event({set_knob_pos,Pos}, #ts{th=Th}=Ts0) ->
    Ts = Ts0#ts{first=trunc(Th*Pos) div ?LINE_HEIGHT},
    update_scroller(Ts),
    get_help_event(Ts);
handle_help_event(scroll_page_up, #ts{wh=Wh}=Ts) ->
    zoom_step(-Wh div ?LINE_HEIGHT, Ts);
handle_help_event(scroll_page_down, #ts{wh=Wh}=Ts) ->
    zoom_step(Wh div ?LINE_HEIGHT, Ts);
handle_help_event(#mousebutton{button=4,state=?SDL_RELEASED}, Ost) ->
    zoom_step(-10, Ost);
handle_help_event(#mousebutton{button=5,state=?SDL_RELEASED}, Ost) ->
    zoom_step(10, Ost);
handle_help_event(_, _) -> keep.

zoom_step(Step, #ts{first=First0,th=Th}=Ts0) ->
    NumLines = Th div ?LINE_HEIGHT,
    First = case First0+Step of
		Neg when Neg < 0 -> 0;
		First1 when First1 < NumLines -> First1;
		_ -> First0
	    end,
    Ts = Ts0#ts{first=First},
    update_scroller(Ts),
    get_help_event(Ts).

redraw(#ts{lines=Lines0,first=First}=Ts) ->
    update_scroller(Ts),
    wings_io:ortho_setup(),
    {W,H} = wings_wm:win_size(),
    wings_io:border(0, 0, W-1, H-1, {1.0,1.0,1.0}),
    gl:translated(4, 4+?LINE_HEIGHT, 0),
    Lines = lists:nthtail(First, Lines0),
    foldl(fun(L, Y) ->
		  wings_io:text_at(5, Y, L),
		  Y+?LINE_HEIGHT
	  end, 0, Lines).

update_scroller(#ts{first=First,th=Th}) ->
    {_,H} = wings_wm:win_size(),
    Name = wings_wm:this(),
    wings_wm:set_knob(Name, First*?LINE_HEIGHT/Th, H/Th).

cmd([M|[_|_]=Ms]) ->
    [{bold,M},$| | cmd(Ms)];
cmd([M]) -> [{bold,M}].


%%%
%%% Help|About (splash screen).
%%%

-define(MARGIN, 12).

about(_) ->
%% \		    /
%%  \		   /		    	         __    	__
%%   \		  /    .	    	   	/  \   |  \
%%    \	   /\	 /     	   __  	 __    __  	  _/   |   |
%%     \  /  \ 	/      |  |  | 	|  |  |__      	   \   |   |
%%	\/    \/       |  |  | 	|__|   __|	\__/   |__/
%%		       	     	 __|
    {Xs0,Ys} = splash_size(),
    Xs = Xs0+2*?MARGIN,
    {W,H} = wings_wm:top_size(),
    X = trunc((W-Xs) / 2),
    Y = trunc((H-Ys) / 2),
    Op = {push,fun handle_splash_event/1},
    wings_wm:delete(help),
    wings_wm:new(help, {X,Y,highest}, {Xs,Ys+?LINE_HEIGHT}, Op),
    wings_wm:grab_focus(help),
    wings_wm:dirty(),
    keep.

handle_splash_event(redraw) ->
    message(),
    wings_io:ortho_setup(),
    {Xs,Ys} = wings_wm:win_size(),
    wings_io:raised_rect(0, 0, Xs, Ys),
    gl:recti(3, 3, Xs-3, Ys-3),
    gl:color3f(1, 1, 1),
    gl:recti(4, 4, Xs-4, Ys-4),
    draw_splash(splash_contents()),
    keep;
handle_splash_event(#mousemotion{}) -> keep;
handle_splash_event(got_focus) -> message();
handle_splash_event(lost_focus) -> keep;
handle_splash_event(_) -> delete.

message() ->
    wings_msg:button(?__(1,"Close help window")),
    keep.

splash_size() ->
    splash_size(splash_contents()).

splash_size(L) ->
    splash_size_1(L, 0, 0).

splash_size_1([{icon,_,W,H}|T], W0, H0) ->
    splash_size_1(T, max(W, W0), H0+H);
splash_size_1([{text,Text}|T], W0, H0) ->
    Tw = wings_text:width(Text),
    splash_size_1(T, max(W0, Tw), H0+wings_text:height()+4);
splash_size_1([{spacer,W,H}|T], W0, H0) ->
    splash_size_1(T, max(W0, W), H0+H);
splash_size_1([], W, H) -> {W,H}.

draw_splash(L) ->
    draw_splash_1(L, 0).

draw_splash_1([{icon,Name,Iw,Ih}|T], Y) ->
    gl:color3f(1, 0, 1),
    {W,_} = wings_wm:win_size(),
    X = W - Iw - ?MARGIN,
    wings_io:draw_icons(fun() -> wings_io:draw_icon(X, Y, Name) end),
    draw_splash_1(T, Y+Ih);
draw_splash_1([{text,Text}|T], Y) ->
    gl:color3b(0, 0, 0),
    Th = wings_text:height(),
    {W,_} = wings_wm:win_size(),
    Tw = wings_text:width(Text),
    X = W - Tw - ?MARGIN,
    wings_io:text_at(X, Y+Th, Text),
    draw_splash_1(T, Y+Th+4);
draw_splash_1([{spacer,_,H}|T], Y) ->
    draw_splash_1(T, Y+H);
draw_splash_1([_|T], Y) ->
    draw_splash_1(T, Y);
draw_splash_1([], _) -> ok.
    
splash_contents() ->
    [{spacer,0,14},
     {icon,about_wings,331,139},
     {text,[{bold,?WINGS_VERSION}]},
     {spacer,0,10},
     {text,?__(1,"Wings 3D is a subdivision modeler inspired")},
     {text,?__(2,"by Nendo and Mirai from IZware.")},
     {spacer,0,10},
     {text,?__(3,"Wings 3D comes with absolutely no warranty,")},
     {text,?__(4,"but is completely free for any kind of use")},
     {text,?__(5,"(including commercial).")},
     {spacer,0,10},
     {text,?__(6,"Copyright") 
      ++ [$\s,169] ++ " 2001-2009 "++"Bj" ++ [246] ++ "rn Gustavsson " ++
      ?__(7,"& Others")},
     {text,?__(8,"JPEG library: Copyright") ++ [$\s,169] ++
      " 1991-1998 Thomas G. Lane"}
    ].

edit_prefs() ->
    cmd([?__(1,"Edit"),
	 ?__(2,"Preferences")]).
    
