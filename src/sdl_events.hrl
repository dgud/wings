%%  Copyright (c) 2001 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : sdl_events.hrl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : Event definitions
%%% Created : 7 Jul 2000 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

-define(SDL_PRESSED,     1).
-define(SDL_RELEASED,    0).

-define(SDL_ALL_HOTKEYS,	16#FFFFFFFF).

-record(keyboard,   %% SDL_KeyboardEvent
	{which,     %% The keyboard device index
	 state,     %% SDL_PRESSED or SDL_RELEASED
	 scancode,  %% hardware specific scancode
	 sym,	    %% SDL virtual keysym      see sdl_keyboard.hrl
	 mod,       %% current key modifiers   see sdl_keyboard.hrl
	 unicode}). %% translated character

-record(mousemotion,%% SDL_MouseMotionEvent
	{which,	  %% The mouse device index
	 state,	  %% The current button state
	 mod=0,	  %% Current key modifiers
	 x, y,	  %% The X/Y coordinates of the mouse
	 xrel,	  %% The relative motion in the X direction
	 yrel}).  %% The relative motion in the Y direction

-record(mousebutton,%% SDL_MouseButtonEvent
	{which,	  %% The mouse device index
	 button,  %% The mouse button index
	 state,   %% SDL_PRESSED or SDL_RELEASED
	 mod=0,	  %% Current key modifiers
	 x, y}).  %% The X/Y coordinates of the mouse at press time

%%%
%% EVENT TYPES and their masks
-define(SDL_NOEVENT,    0).    %% No (new) event
-define(SDL_ACTIVEEVENT,1).    %% Application loses/gains visibility
-define(SDL_ACTIVEEVENTMASK,   (1 bsl ?SDL_ACTIVEEVENT)).
-define(SDL_KEYDOWN,	2).    %% Keys pressed
-define(SDL_KEYDOWNMASK,       (1 bsl ?SDL_KEYDOWN)).
-define(SDL_KEYUP,	3).    %% Keys released
-define(SDL_KEYUPMASK,         (1 bsl ?SDL_KEYUP)).
-define(SDL_MOUSEMOTION,4).    %% Mouse moved
-define(SDL_MOUSEMOTIONMASK,   (1 bsl ?SDL_MOUSEMOTION)).
-define(SDL_MOUSEBUTTONDOWN,5).%% Mouse button pressed
-define(SDL_MOUSEBUTTONDOWNMASK,(1 bsl ?SDL_MOUSEBUTTONDOWN)).
-define(SDL_MOUSEBUTTONUP,6).  %% Mouse button released
-define(SDL_MOUSEBUTTONUPMASK, (1 bsl ?SDL_MOUSEBUTTONUP)).

-define(SDL_MOUSEEVENTMASK,    (?SDL_MOUSEBUTTONUPMASK bor
				?SDL_MOUSEBUTTONDOWNMASK bor
				?SDL_MOUSEMOTIONMASK)).

-define(SDL_KEYBOARDMASK,      (?SDL_KEYDOWNMASK bor
				?SDL_KEYUPMASK)).
-define(SDL_MOUSEBUTTONMASK,   (?SDL_MOUSEBUTTONDOWNMASK bor
				?SDL_MOUSEBUTTONUPMASK)).

-define(SDL_ALLEVENTS, 16#FFFFFFFF).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

