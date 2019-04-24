%%
%%  wings_help.erl --
%%
%%     This module implements the Help menu.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_help).
-export([menu/0,command/2]).
-export([cmd/1,help_window/2,no_more_basic_menus/0,
	 not_possible_to_save_prefs/0, about_panel/2]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

menu() ->
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
    ?__(10,"Software/OpenGL Info");
header(about) ->
    ?__(11,"About Wings 3D");
header(tweak_help) ->
    ?__(12,"Tweak").

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
command_1(tweak_help, Header) ->
    Msg = wings_tweak:help_msg(),
    help_window(Header, Msg);
command_1(about, Header) ->
    about(Header).

no_more_basic_menus() ->
    Info =
	["You are seeing this message because the Advanced pop-up"
	 "menus option was turned off in your preferences.",
	 "From now on, Wings will only support Advanced pop-up menus.",
	 "The major difference compared to the Basic pop-up menus, "
	 "which you have been using, is that all three mouse buttons"
	 " can be used to execute a command.",
	 "Usually, the most common variation of a command is accessed by "
	 "clicking the left mouse button, while the middle and right mouse "
	 "buttons provide more advanced options for the command.",
	 "Details about commands activated by each mouse button are displayed"
	 "in the Information Line across the bottom of your screen when you "
	 "hover over any menu item."],
    wings_dialog:info("Basic Menus", Info, []).

not_possible_to_save_prefs() ->
    Info = ["The Wings3D.exe program was not able to locate the folder "
	    "for applications settings.",
	    "This is not supposed to happen. "
	    "You should report this bug."],
    wings_dialog:info("Error", Info, []).

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
	 "you must inform Wings of how many buttons "
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
    H = [?__(1,"The performance of Wings is dependent on many different things, such as"),
	 {bullet,
	  [?__(2,"the speed of the CPU"),
	   ?__(3,"type and size of the CPU cache"),
	   ?__(4,"amount and speed of memory"),
	   ?__(5,"type of graphics card"),
	   ?__(6,"amount of video memory"),
	   ?__(7,"the phase of the moon") ++
	       ?__(8,"Therefore, it is difficult to give any firm advice on how to improve Wings performance. The following tips MAY improve performance:"),

	   ?__(31,"Use fewer Undo levels. Setting to 4 or less (default 32) will let you work on models with almost 8 times the detail. In this case, using a shorter autosave period (default 10mins) may be advisable."),

	   ?__(9,"Try different number of colors and different screen resolutions. Especially if the graphics card doesn't have much memory, many colors and/or high resolution may drastically reduce performance. Using a smaller Wings window (not maximized) may also help."),

	   ?__(10,"Close unnecessary windows inside Wings."),

	   ?__(11,"Make sure that Geometry windows don't overlap."),

	   ?__(12,"Use as few (active) lights as possible. More lights means less speed on most grahics cards."),

	   ?__(13,"If possible, use the ")
	   ++cmd([?__(14,"Tools"),
		  ?__(15,"Virtual Mirror")])
	   ++?__(16," command."),

	   ?__(17,"Hide models that you don't work on for the moment."),

	   ?__(18,"Use the ")
	   ++cmd([?__(19,"View"),
		  ?__(20,"Show Colors")])
	   ++?__(21," command to turn off vertex color display if your model has vertex colors."),

	   ?__(22,"Use the ")
	   ++cmd([?__(23,"View"),
		  ?__(24,"Show Textures")])
	   ++?__(25," command to turn off textures while modeling."),

	   ?__(26,"Work in wireframe mode."),

	   ?__(27,"Some graphics cards display edges slowly. Turn off edge display using the ")
	   ++cmd([?__(28,"View"),
		  ?__(29,"Show Edges")])
	   ++?__(30," command.")
	  ]}],
    help_window(Head, H).

hotkeys(Head) ->
    Help = wings_hotkey:listing(),
    help_window(Head, Help).

def_hotkeys(Head) ->
    Ctrl = case os:type() of
	       {unix, darwin} -> command;
	       _ -> ctrl
	   end,
    Insert0 = wings_hotkey:format_hotkey({?SDLK_INSERT,[Ctrl]},pretty),
    Insert1 = wings_hotkey:format_hotkey({$8, [Ctrl]}, pretty),
    Delete0 = wings_hotkey:format_hotkey({?SDLK_DELETE,[Ctrl]},pretty),
    Delete1 = wings_hotkey:format_hotkey({$9, [Ctrl]}, pretty),

    Help = [?__(1,"Any command that appears in a menu, can be assigned a keyboard short-cut (hotkey)."),
	    ?__(4,"To assign a hotkey to a command:"),
	    io_lib:format(?__(6,"1. Press ~ts or ~ts key."), [Insert0,Insert1]),
	    ?__(5,"2. Select (with the correct mouse button) the menu item of the command"),
	    ?__(8,"3. The information line asks you to press the key that the command "
		"should be assigned to."),

	    io_lib:format(?__(9,"To delete a hotkey, similarly press the "
			      "~ts or ~ts key and "
			      "select the command in a menu. A dialog box listing all keys "
			      "bound to the command will appear. "
			      "Check all hotkeys you want to delete."), [Delete0,Delete1])],
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
    Versions = {table, 2, "Version information",
		[[?__(1,"Vendor"),gl:getString(?GL_VENDOR)],
		 [?__(2,"Renderer"), gl:getString(?GL_RENDERER)],
		 [?__(3,"Version"),gl:getString(?GL_VERSION)],
		 [?__(4,"Version tuple"), lists:flatten(io_lib:format("~p\n", [VerTuple]))],
		 [?__(400, "2D-API:"), wings_io:version_info()]]},
        GL = {table, 2, "OpenGL Information",
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
			 ?GL_MAX_ELEMENTS_VERTICES}]) ++
		  get_info([{?__(41,"Max number of texturing units"),?GL_MAX_TEXTURE_UNITS},
			    {?__(42,"Number of compression formats"),
			     ?GL_NUM_COMPRESSED_TEXTURE_FORMATS},
			    {?__(43,"Max number of vertex units"),?GL_MAX_VERTEX_UNITS_ARB}])
	      },
    Extensions = [{bullet,extensions()}],
    Help = [Versions, GL, [{bold,?__(44,"OpenGL Extensions")}]] ++ Extensions,
    help_window(Head, Help).

get_info([{Label,Attr}|T]) ->
    Val = gl:getIntegerv(Attr),
    ValStr = case {gl:getError(),Val} of
		 {0,List} ->
		     get_info_1(Attr, List);
		 _ -> "---"
	     end,
    [[Label, ValStr]|get_info(T)];
get_info([]) -> [].

get_info_1(_, [A]) -> integer_to_list(A);
get_info_1(Enum, [A,B|_]) ->
    case has_one_elem(Enum) orelse (B =:= 0) of
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

extensions([H|T]) -> [H|extensions(T)];
extensions([]) -> [].

%%%
%%% Scrollable help window.
%%%

help_window(Title, []) ->
    help_window(Title, [?__(1,"No help text")]);
help_window(Title, Text) ->
    help_window(help, Title, Text).
help_window(_Name, Title, Text) ->
    wings_dialog:info(Title, Text, []),
    keep.

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
    Flags  = [{style, ?wxCAPTION bor ?wxRESIZE_BORDER bor ?wxCLOSE_BOX}],
    Frame = wxMiniFrame:new(?GET(top_frame), ?wxID_ANY, ?__(1, "About"), Flags),
    Icons = wings_frame:get_icon_images(),
    {_Panel,Szr} = about_panel(Frame, Icons),
    wxSizer:setSizeHints(Szr, Frame),
    wxWindow:centerOnParent(Frame),
    wxWindow:show(Frame),
    wxWindow:setFocus(Frame),
    keep.

about_panel(Parent, Imgs) ->
    %% the border shows a nice splash window when Wings3d starts
    Panel = wxPanel:new(Parent,[{style,?wxBORDER_SIMPLE}]),
    SzrMain = wxBoxSizer:new(?wxVERTICAL),

    {_, {W,H}, Img} = lists:keyfind(about_wings, 1, Imgs),

    SzrTop = wxBoxSizer:new(?wxVERTICAL),
    TopPanel = wxPanel:new(Panel, [{size,{W,H}}]),
    wxSizer:add(SzrTop, TopPanel,
		[{proportion,0}, {flag, ?wxALIGN_TOP}, {border,0}]),
    wxSizer:add(SzrMain, SzrTop, [{proportion,0}, {flag,0}, {border,1}]),


    {_, _, ImgArt} = lists:keyfind(about_wings_art, 1, Imgs),
    SzrMiddle = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(SzrMiddle, wxStaticBitmap:new(Panel, ?wxID_ANY, wxBitmap:new(ImgArt)),
		[{proportion,0}, {flag, ?wxALL}, {border,3}]),
    wxSizer:add(SzrMain, SzrMiddle, [{proportion,0}, {flag,0}, {border,1}]),


    SzrBottomCol1 = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(SzrBottomCol1, wxStaticText:new(Panel, ?wxID_ANY, splash_content(bottom_left),
						[{size,{340,H}}]),
		[{proportion,0}, {flag, ?wxEXPAND bor ?wxLEFT}, {border, 5}]),
    wxSizer:addSpacer(SzrBottomCol1,30),
    wxSizer:add(SzrBottomCol1, wxStaticText:new(Panel, ?wxID_ANY, splash_content(bottom_right),
						[{style, ?wxALIGN_RIGHT}]),
		[{proportion,1}, {flag, ?wxEXPAND bor ?wxRIGHT}, {border, 5}]),
    SzrBottom = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(SzrBottom, SzrBottomCol1, [{proportion,2}, {flag,?wxEXPAND}, {border,5}]),

    wxSizer:add(SzrMain, SzrBottom, [{proportion,0}, {flag,?wxALL bor ?wxBOTTOM bor ?wxEXPAND}, {border,0} ]),
    wxPanel:setSizer(Panel, SzrMain),
    wxPanel:layout(Panel),
    %% used to allow us to draw the header text over the StaticBitmap
    wxWindow:connect(TopPanel, paint, [{callback, fun redraw/2}, {userData,Img}]),
    {Panel, SzrMain}.

-define(lblVerX, 180).
-define(lblVerY, 27).
-define(lblDesX, 48).
redraw(#wx{obj=TopPanel, event=#wxPaint{}, userData=Img}, _) ->
    String = splash_content(top),
    Www = "www.wings3d.com",
    {MaxW,MaxH} = wxWindow:getSize(TopPanel),

    DC = case os:type() of
	     {win32, _} -> %% Flicker on windows
		 wx:typeCast(wxBufferedPaintDC:new(TopPanel), wxPaintDC);
	     _ ->
		 wxPaintDC:new(TopPanel)
	 end,
    wxDC:setBackground(DC, ?wxWHITE_BRUSH),
    wxDC:clear(DC),

    GC = wxGraphicsContext:create(DC),
    %% Drawing the top image background which will get the Wings3D's version stamped over
    BM = wxBitmap:new(Img),
    wxGraphicsContext:drawBitmap(GC, BM, 0, 0, wxBitmap:getWidth(BM), wxBitmap:getHeight(BM)),
    wxBitmap:destroy(BM),

    %% Prepating to use transparent color for the text background
    BGB = wxBrush:new({0,0,0}, [{style, ?wxTRANSPARENT}]),
    wxGraphicsContext:setBrush(GC, BGB),

    %% Drawing the version number
    Font1 = wxFont:new(11, ?wxDEFAULT, ?wxNORMAL, ?wxFONTWEIGHT_BOLD),
    wxGraphicsContext:setFont(GC, Font1, {200,200,200}),
    {_,H0,_,_} = wxGraphicsContext:getTextExtent(GC, "v2"),
    wxGraphicsContext:drawText(GC, "v" ++ ?WINGS_VERSION, ?lblVerX, ?lblVerY-H0),
    wxFont:destroy(Font1),

    %% Drawing Wings3D description
    Font2 = wxFont:new(8, ?wxDEFAULT, ?wxNORMAL, ?wxFONTWEIGHT_NORMAL),
    wxGraphicsContext:setFont(GC, Font2, {200,200,200}),
    {W,H,_,_} = wxGraphicsContext:getTextExtent(GC, Www),
    wxGraphicsContext:drawText(GC, String, ?lblDesX, MaxH-H-3),
    wxGraphicsContext:drawText(GC, Www, MaxW-W-5, MaxH-H-3),
    wxFont:destroy(Font2),

    wxBrush:destroy(BGB),
    wxPaintDC:destroy(DC),
    ok.

splash_content(top) ->
    [Top,_,_] = splash_contents(),
    Top;
splash_content(bottom_left) ->
    [_,Left,_] = splash_contents(),
    Left;
splash_content(bottom_right) ->
    [_,_,Right] = splash_contents(),
    Right.

splash_contents() ->
    [?__(1,"Wings 3D is a subdivision modeler inspired") ++ " " ++
     ?__(2,"by Nendo and Mirai from IZware."),

     ?__(3,"Wings 3D comes with absolutely no warranty,") ++ " " ++
     ?__(4,"but is completely free for any kind of use") ++ " " ++
     ?__(5,"(including commercial)."),

     ?__(6,"Copyright") ++ [$\s,169] ++ " 2001-2019 Björn Gustavsson" ++ "\n" ++
     "Dan Gudmundsson" ++ ?__(7," and Others")
    ].

edit_prefs() ->
    cmd([?__(1,"Edit"),
	 ?__(2,"Preferences")]).
    
