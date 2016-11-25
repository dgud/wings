%%
%%  wings_theme.erl --
%%
%%     Canned themes for the interface.
%%
%%  Copyright (c) 2010-2011 Richard Jones
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wings_theme).
-export([menu/0, native_theme/0,
	 legacy_colors/0,
	 grey_blue_theme/0,olive_theme/0,dark_blue_theme/0]).

-export([sys_colors/0]).

-include("wings.hrl").

menu() ->
    Help = ?__(7,"Change color scheme"),
    {?__(1,"Themes"),
     {theme,
      [{?__(10,"Native Theme"), native_theme,
	?__(11,"Change color scheme to colors that tries to mimic the OS")},
       {?__(2,"Classic Green Theme"),legacy_colors,
        ?__(3,"Change the display colors to the original Wings3D theme with the green titlebars")},
       {?__(4,"Olive Theme"),olive_theme,Help},
       {?__(5,"Grey Blue Theme"),grey_blue_theme,Help},
       {?__(6,"Dark Blue Theme"),dark_blue_theme,Help}]}}.

%%%
%%% Native theme
%%% Use system color where possible (and working)
%%%

native_theme() ->
    Workspace = [{active_vector_color,{0.0,1.0,0.0}},
		 {clip_plane_color,{0.8,0.3,0.0}},
		 {console_color, {1.0,1.0,1.0}},
		 {console_text_color,{0.0,0.0,0.0}},
		 {default_axis,{{0.0,0.0,0.0},{1.0,0.0,0.0}}},
		 {edge_color,{0.0,0.0,0.0}},
		 {grid_color,{0.3,0.3,0.3}},
		 {hard_edge_color,{1.0,0.5,0.0}},
		 {info_background_color,{0.38,0.38,0.38,0.5}},
		 {info_color,{1.0,1.0,1.0}},
		 {info_line_bg,{0.33131360000000004,0.4,0.0}},
		 {info_line_text,{1.0,1.0,1.0}},
		 {masked_vertex_color,{0.5,1.0,0.0,0.8}},
		 {material_default, {0.7898538076923077,0.8133333333333334,0.6940444444444445}},
		 {neg_x_color,{0.8,0.8,0.8}},
		 {neg_y_color,{0.8,0.8,0.8}},
		 {neg_z_color,{0.8,0.8,0.8}},
		 {normal_vector_color,{0.0,1.0,0.0}},
		 {sculpt_magnet_color,{0.0,0.0,1.0,0.1}},
		 {selected_color,{0.65,0.0,0.0}},
		 {selected_hlite,{0.7,0.7,0.0}},
		 {tweak_magnet_color,{0.0,0.0,1.0,0.06}},
		 {tweak_vector_color,{1.0,0.5,0.0}},
		 {unselected_hlite,{0.0,0.65,0.0}},
		 {vertex_color,{0.0,0.0,0.0}},
		 {x_color,{0.7,0.0,0.1}},
		 {y_color,{0.37210077142857145,0.82,0.0}},
		 {z_color,{0.0,0.3,0.8}}],
    UserIF = [{background_color, appworkspace},
	      {dialog_color, desktop},
	      {dialog_disabled, graytext},
	      {dialog_text, windowtext},
	      {menu_bar_bg,  menubar},
	      {menubar_text, menutext},
	      {menu_color,  menu},
	      {menu_hilite, menuhilight},
	      {menu_hilited_text,highlighttext},
	      {menu_text, menutext},
	      {outliner_geograph_bg, menu},
	      {outliner_geograph_disabled, graytext},
	      {outliner_geograph_hl,      highlight},
	      {outliner_geograph_hl_text, highlighttext},
	      {outliner_geograph_text, windowtext},
	      {title_active_color, activecaption},
	      {title_passive_color,inactivecaption},
	      {title_text_color,   captiontext}
	     ],
    OS = element(2, os:type()),
    Version = os:version(),
    wx:new(),
    [native(Opt, OS, Version) || Opt <- UserIF] ++ [{flat_color_panels, true}| Workspace].

%% Windows Versions
%%    XP      5.1
%%    Vista   6.0
%%    7       6.1
%%    8(.1)   6.2 (6.3)
%%    10     10.0

%% On linux it hard to get right color comes from themes and
%% wxWidgets seems to have a hard time to find them.

native({What=background_color, _}, linux, _) ->
    {What, {0.8,0.8,0.8}};
native({What=menu_color, menu}, linux, _Ver) ->
    {What, wings_color:rgb4fv(wxSystemSettings:getColour(wx_id(menubar)))};
native({What, MenuText}, linux, _)
  when MenuText =:= menubar_text; MenuText =:= menutext ->
    {What, case wings_color:rgb_to_hsv(default(menubar)) of
	       {_, _, V} when V > 0.5 -> {0.0,0.0,0.0};
	       _ -> {1.0,1.0,1.0}
	   end};
native({What=menu_hilite, menuhilight}, darwin, Ver) ->
    native({What, highlight}, darwin, Ver);
native({What, activecaption}, darwin, Ver) ->
    native({What, btnhighlight}, darwin, Ver);
native({What=outliner_geograph_bg, Suggestion}, _, _) ->
    {What, wings_color:rgb4fv(wxSystemSettings:getColour(wx_id(Suggestion)))};
native({What=menu_color, Suggestion}, nt, _) ->
    {What, wings_color:rgb4fv(wxSystemSettings:getColour(wx_id(Suggestion)))};
native({What, Suggestion}, _, _) ->
    {What, default(Suggestion)}.

default(Suggestion) ->
    wings_color:rgb3fv(wxSystemSettings:getColour(wx_id(Suggestion))).

%%%
%%% Legacy Colors - Classic Green Theme
%%%

legacy_colors() ->
    [{flat_color_panels, false},
     {background_color,{0.8,0.8,0.8}},
     {info_color,{0.0,0.0,0.0}},
     {info_background_color,{0.8,0.8,0.8,0.5}},
     {grid_color,{0.7,0.7,0.7}},
     {edge_color,{0.0,0.0,0.0}},
     {hard_edge_color,{1.0,0.5,0.0}},
     {selected_color,{0.65,0.0,0.0}},
     {unselected_hlite,{0.0,0.65,0.0}},
     {selected_hlite,{0.70,0.70,0.0}},
     {x_color,{0.6,0.0,0.0}},
     {y_color,{0.0,0.6,0.0}},
     {z_color,{0.0,0.0,0.6}},
     {neg_x_color,{0.6,0.6,0.6}},
     {neg_y_color,{0.6,0.6,0.6}},
     {neg_z_color,{0.6,0.6,0.6}},
     {vertex_color,{0.0,0.0,0.0}},
     {masked_vertex_color,{0.5,1.0,0.0,0.8}},
     {active_vector_color,{0.0,0.0,0.65}},
     {normal_vector_color,{0.0,0.0,0.65}},
     {material_default,{1.0,1.0,1.0}},
     {clip_plane_color,{0.8,0.3,0.0}},

     %% User interface preferences.
     {menu_text,{0.0,0.0,0.0}},
     {menu_hilite,{0.0,0.0,0.5}},
     {menu_hilited_text,{1.0,1.0,1.0}},
     {menu_color,{0.75,0.75,0.75,0.99}},
     {dialog_text,{0.0,0.0,0.0}},
     {dialog_disabled,{0.5,0.5,0.5}},
     {dialog_color,{0.75,0.75,0.75,0.99}},
     {title_active_color,{0.41,0.55,0.41,1.0}},
     {title_passive_color,{0.325,0.4,0.325,1.0}},
     {title_text_color,{1.0,1.0,1.0}},
     {menu_bar_bg,{0.52,0.52,0.52}},
     {menubar_text,{0.0,0.0,0.0}},
     {info_line_bg,{0.52,0.52,0.52}},
     {info_line_text,{1.0,1.0,1.0}},

     %% Console
     {console_color, {1.0,1.0,1.0}},
     {console_text_color,{0.0,0.0,0.0}},

     %% Outliner and Geometry Graph
     {outliner_geograph_bg,{0.52,0.52,0.52,1.0}},
     {outliner_geograph_text,{0.0,0.0,0.0}},
     {outliner_geograph_hl,{0.0,0.0,0.5}},
     {outliner_geograph_hl_text,{1.0,1.0,1.0}},
     {outliner_geograph_disabled,{0.4,0.4,0.4}}].

%%%
%%% GreyBlue
%%%

grey_blue_theme() ->
    [{flat_color_panels, false},
     {active_vector_color,{0.0,1.0,0.0}},
     {background_color,{0.45,0.4499996641793551,0.4499996641793551}},
     {clip_plane_color,{0.8,0.3,0.0}},
     {console_color, {1.0,1.0,1.0}},
     {console_text_color,{0.11200000000000002,0.3932173913043481,0.7}},
     {default_axis,{{0.0,0.0,0.0},{1.0,0.0,0.0}}},
     {dialog_color,
         {0.5270222222222222,0.5873069411419368,0.6533333333333333,0.9}},
     {dialog_disabled,{0.44,0.44,0.44}},
     {dialog_text,{0.0,0.0,0.0}},
     {edge_color,{0.0,0.0,0.0}},
     {grid_color,{0.16,0.16,0.16}},
     {hard_edge_color,{1.0,0.5,0.0}},
     {info_background_color,{0.3,0.2999996923080079,0.2999996923080079,0.5}},
     {info_color,{1.0,1.0,1.0}},
     {info_line_bg,{0.11200000000000002,0.3932173913043481,0.7}},
     {info_line_text,{1.0,1.0,1.0}},
     {masked_vertex_color,{0.5,1.0,0.0,0.8}},
     {material_default,{0.8200000000000001,0.9049993849536925,1.0}},
     {menu_bar_bg,{0.45,0.4499996641793551,0.4499996641793551}},
     {menu_color,
         {0.5270222222222222,0.5873069411419368,0.6533333333333333,0.9}},
     {menu_hilite,{0.11200000000000002,0.3932173913043481,0.7}},
     {menu_hilited_text,{1.0,1.0,1.0}},
     {menu_text,{0.0,0.0,0.0}},
     {menubar_text,{0.0,0.0,0.0}},
     {neg_x_color,{1.0,1.0,1.0}},
     {neg_y_color,{1.0,1.0,1.0}},
     {neg_z_color,{1.0,1.0,1.0}},
     {normal_vector_color,{0.0,1.0,0.0}},
     {outliner_geograph_bg,
         {0.5270222222222222,0.5873069411419368,0.6533333333333333,0.9}},
     {outliner_geograph_disabled,
         {0.7303529411764705,0.7449347290300067,0.7607843137254902}},
     {outliner_geograph_hl,{0.11200000000000002,0.3932173913043481,0.7}},
     {outliner_geograph_hl_text,{1.0,1.0,1.0}},
     {outliner_geograph_text,{0.0,0.0,0.0}},
     {sculpt_magnet_color,{0.0,0.0,1.0,0.1}},
     {selected_color,{0.6,0.06274509803921569,0.06274509803921569}},
     {selected_hlite,
         {0.6933330093457943,0.6933333333333334,0.05447619047619052}},
     {title_active_color,{0.39013333333333333,0.5670260869565219,0.76,0.9}},
     {title_passive_color,
         {0.5270222222222222,0.5873069411419368,0.6533333333333333,0.9}},
     {title_text_color,{1.0,1.0,1.0}},
     {tweak_magnet_color,{0.0,0.0,1.0,0.06}},
     {tweak_vector_color,{1.0,0.5,0.0}},
     {unselected_hlite,
         {0.05393868421052637,0.6533333333333333,0.053938768489852164}},
     {vertex_color,{0.0,0.0,0.0}},
     {x_color,{0.74,0.06274509803921569,0.06274509803921569}},
     {y_color,{0.062745,0.76,0.06274509803921569}},
     {z_color,{0.062745,0.06274509803921569,0.76}}].

%%%
%%% Olive Green theme
%%%

olive_theme() ->
    [{flat_color_panels, false},
     {active_vector_color,{0.0,1.0,0.0}},
     {background_color,{0.5599999999999999,0.56,0.56}},
     {clip_plane_color,{0.8,0.3,0.0}},
     {console_color, {1.0,1.0,1.0}},
     {console_text_color,{0.0,0.0,0.0}},
     {console_text_color,{0.0,1.0,0.0}},
     {default_axis,{{0.0,0.0,0.0},{1.0,0.0,0.0}}},
     {dialog_color,{0.68,0.68,0.68,1.0}},
     {dialog_disabled,
         {0.46999999999999986,0.46999999999999986,0.46999999999999986}},
     {dialog_text,{0.0,0.0,0.0}},
     {edge_color,{0.0,0.0,0.0}},
     {grid_color,{0.3,0.3,0.3}},
     {hard_edge_color,{1.0,0.5,0.0}},
     {info_background_color,{0.38,0.38,0.38,0.5}},
     {info_color,{1.0,1.0,1.0}},
     {info_line_bg,{0.33131360000000004,0.4,0.0}},
     {info_line_text,{1.0,1.0,1.0}},
     {masked_vertex_color,{0.5,1.0,0.0,0.8}},
     {material_default,
         {0.7898538076923077,0.8133333333333334,0.6940444444444445}},
     {menu_bar_bg,{0.68,0.68,0.68}},
     {menu_color,{0.678431,0.6784313725490196,0.6784313725490196,1.0}},
     {menu_hilite,{0.33131360000000004,0.4,0.0}},
     {menu_hilited_text,{1.0,1.0,1.0}},
     {menu_text,{0.0,0.0,0.0}},
     {menubar_text,{0.0,0.0,0.0}},
     {neg_x_color,{0.8,0.8,0.8}},
     {neg_y_color,{0.8,0.8,0.8}},
     {neg_z_color,{0.8,0.8,0.8}},
     {normal_vector_color,{0.0,1.0,0.0}},
     {outliner_geograph_bg,{0.68,0.68,0.68,1.0}},
     {outliner_geograph_disabled,
         {0.5066662941176471,0.5066666666666667,0.5066666666666667}},
     {outliner_geograph_hl,{0.4141418073872209,0.5,0.0}},
     {outliner_geograph_hl_text,{1.0,1.0,1.0}},
     {outliner_geograph_text,{0.0,0.0,0.0}},
     {sculpt_magnet_color,{0.0,0.0,1.0,0.1}},
     {selected_color,{0.65,0.0,0.0}},
     {selected_hlite,{0.7,0.7,0.0}},
     {title_active_color,{0.647059,0.7137254901960784,0.3254901960784314,1.0}},
     {title_passive_color,{0.4141418073872209,0.5,0.0,1.0}},
     {title_text_color,{1.0,1.0,1.0}},
     {tweak_magnet_color,{0.0,0.0,1.0,0.06}},
     {tweak_vector_color,{1.0,0.5,0.0}},
     {unselected_hlite,{0.0,0.65,0.0}},
     {vertex_color,{0.0,0.0,0.0}},
     {x_color,{0.7,0.0,0.1}},
     {y_color,{0.37210077142857145,0.82,0.0}},
     {z_color,{0.0,0.3,0.8}}].

%%%
%%% Dark Blue theme
%%%

dark_blue_theme() ->
    [{flat_color_panels, false},
     {active_vector_color,{0.0,1.0,0.0}},
     {background_color,{0.2,0.2,0.2}},
     {clip_plane_color,{0.8,0.3,0.0}},
     {console_color, {1.0,1.0,1.0}},
     {console_text_color,{0.07,0.4,0.76}},
     {default_axis,{{0.0,0.0,0.0},{1.0,0.0,0.0}}},
     {dialog_color,{0.13,0.13,0.13,0.9}},
     {dialog_disabled,{0.32,0.32,0.32}},
     {dialog_text,{0.76,0.76,0.76}},
     {edge_color,{0.0,0.0,0.0}},
     {grid_color,{0.3,0.3,0.3}},
     {hard_edge_color,{1.0,0.5,0.0}},
     {hl_lightpos,{3.0,10.0,1.0}},
     {info_background_color,{0.15,0.46,0.8,0.5}},
     {info_color,{1.0,1.0,1.0}},
     {info_line_bg,{0.05,0.27,0.56}},
     {info_line_text,{1.0,1.0,1.0}},
     {masked_vertex_color,{0.5,1.0,0.0,0.8}},
     {material_default,{0.8333333333333334,0.7428571428571428,
                        0.6444444444444445}},
     {menu_bar_bg,{0.32,0.32,0.32}},
     {menu_color,{0.13,0.13,0.13,0.8}},
     {menu_hilite,{0.07,0.4,0.76}},
     {menu_hilited_text,{1.0,1.0,1.0}},
     {menu_text,{0.76,0.76,0.76}},
     {menubar_text,{1.0,1.0,1.0}},
     {neg_x_color,{0.8,0.8,0.8}},
     {neg_y_color,{0.8,0.8,0.8}},
     {neg_z_color,{0.8,0.8,0.8}},
     {normal_vector_color,{0.0,1.0,0.0}},
     {outliner_geograph_bg,{0.32,0.32,0.32,0.65}},
     {outliner_geograph_disabled,{0.2,0.2,0.2}},
     {outliner_geograph_hl,{0.07,0.4,0.76}},
     {outliner_geograph_hl_text,{1.0,1.0,1.0}},
     {outliner_geograph_text,{0.76,0.76,0.76}},
     {proxy_moving_opacity,1.0},
     {proxy_shaded_edge_style,some},
     {proxy_static_opacity,1.0},
     {sculpt_magnet_color,{0.0,0.0,1.0,0.1}},
     {selected_color,{0.65,0.0,0.0}},
     {selected_edge_width,4.0},
     {selected_hlite,{0.7,0.7,0.0}},
     {title_active_color,{0.07,0.4,0.7,0.9}},
     {title_passive_color,{0.05,0.27,0.5,0.9}},
     {title_text_color,{1.0,1.0,1.0}},
     {tweak_magnet_color,{0.0,0.0,1.0,0.06}},
     {tweak_vector_color,{1.0,0.5,0.0}},
     {unselected_hlite,{0.0,0.65,0.0}},
     {vertex_color,{0.0,0.0,0.0}},
     {x_color,{0.75,0.0,0.0}},
     {y_color,{0.0,0.75,0.0}},
     {z_color,{0.0,0.4,0.75}}].


wx_id(Name) ->
    case Name of
	scrollbar -> ?wxSYS_COLOUR_SCROLLBAR;
	background -> ?wxSYS_COLOUR_BACKGROUND;
	desktop -> ?wxSYS_COLOUR_DESKTOP;
	activecaption -> ?wxSYS_COLOUR_ACTIVECAPTION;
	inactivecaption -> ?wxSYS_COLOUR_INACTIVECAPTION;
	menu -> ?wxSYS_COLOUR_MENU;
	window -> ?wxSYS_COLOUR_WINDOW;
	windowframe -> ?wxSYS_COLOUR_WINDOWFRAME;
	menutext -> ?wxSYS_COLOUR_MENUTEXT;
	windowtext -> ?wxSYS_COLOUR_WINDOWTEXT;
	captiontext -> ?wxSYS_COLOUR_CAPTIONTEXT;
	activeborder -> ?wxSYS_COLOUR_ACTIVEBORDER;
	inactiveborder -> ?wxSYS_COLOUR_INACTIVEBORDER;
	appworkspace -> ?wxSYS_COLOUR_APPWORKSPACE;
	highlight -> ?wxSYS_COLOUR_HIGHLIGHT;
	highlighttext -> ?wxSYS_COLOUR_HIGHLIGHTTEXT;
	btnface -> ?wxSYS_COLOUR_BTNFACE;
	c3dface -> ?wxSYS_COLOUR_3DFACE;
	btnshadow -> ?wxSYS_COLOUR_BTNSHADOW;
	c3dshadow -> ?wxSYS_COLOUR_3DSHADOW;
	graytext -> ?wxSYS_COLOUR_GRAYTEXT;
	btntext -> ?wxSYS_COLOUR_BTNTEXT;
	inactivecaptiontext -> ?wxSYS_COLOUR_INACTIVECAPTIONTEXT;
	btnhighlight ->  ?wxSYS_COLOUR_BTNHIGHLIGHT;
	btnhilight -> ?wxSYS_COLOUR_BTNHILIGHT;
	c3dhighlight ->  ?wxSYS_COLOUR_3DHIGHLIGHT;
	c3dhilight -> ?wxSYS_COLOUR_3DHILIGHT;
	c3ddkshadow ->  ?wxSYS_COLOUR_3DDKSHADOW;
	c3dlight -> ?wxSYS_COLOUR_3DLIGHT;
	infotext ->  ?wxSYS_COLOUR_INFOTEXT;
	infobk ->  ?wxSYS_COLOUR_INFOBK;
	listbox -> ?wxSYS_COLOUR_LISTBOX;
	hotlight ->  ?wxSYS_COLOUR_HOTLIGHT;
	gradientactivecaption -> ?wxSYS_COLOUR_GRADIENTACTIVECAPTION;
	gradientinactivecaption ->  ?wxSYS_COLOUR_GRADIENTINACTIVECAPTION;
	menuhilight -> ?wxSYS_COLOUR_MENUHILIGHT;
	menubar ->  ?wxSYS_COLOUR_MENUBAR;
	listboxtext -> ?wxSYS_COLOUR_LISTBOXTEXT;
	listboxhighlighttext ->  ?wxSYS_COLOUR_LISTBOXHIGHLIGHTTEXT
    end.

sys_colors() ->
    All = [scrollbar, background, desktop, activecaption,
	   inactivecaption, menu, window, windowframe, menutext, windowtext,
	   captiontext, activeborder, inactiveborder, appworkspace,
	   highlight, highlighttext, btnface, c3dface, btnshadow, c3dshadow,
	   graytext, btntext, inactivecaptiontext, btnhighlight, btnhilight,
	   c3dhighlight, c3dhilight, c3ddkshadow, c3dlight, infotext, infobk,
	   listbox, hotlight, gradientactivecaption, gradientinactivecaption,
	   menuhilight, menubar, listboxtext, listboxhighlighttext],
    Frame = wxFrame:new(wx:new(), -1, "System colors", []),
    Sizer = wxGridSizer:new(4),
    lists:foreach(fun(Id) ->
			  wxSizer:add(Sizer, wxStaticText:new(Frame, -1, atom_to_list(Id))),
			  Color = wxSystemSettings:getColour(wx_id(Id)),
			  wxSizer:add(Sizer, wxColourPickerCtrl:new(Frame, -1, [{col, Color}]))
		  end, lists:sort(All)),

    wxWindow:setSizerAndFit(Frame, Sizer),
    wxFrame:show(Frame),
    true.
