%%  Copyright (c) 2001 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : sdl_keyboard.hrl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : Define the available keys 
%%% Created : 7 Jul 2000 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

-define(SDL_DEFAULT_REPEAT_DELAY,	500).
-define(SDL_DEFAULT_REPEAT_INTERVAL,	30).

-define(SDLK_UNKNOWN		, 0).
-define(SDLK_FIRST		, 0).
-define(SDLK_BACKSPACE		, 8).
-define(SDLK_TAB		, 9).
-define(SDLK_CLEAR		, 12).
-define(SDLK_RETURN		, 13).
-define(SDLK_PAUSE		, 19).
-define(SDLK_ESCAPE		, 27).
-define(SDLK_SPACE		, 32).
-define(SDLK_EXCLAIM		, 33).
-define(SDLK_QUOTEDBL		, 34).
-define(SDLK_HASH		, 35).
-define(SDLK_DOLLAR		, 36).
-define(SDLK_AMPERSAND		, 38).
-define(SDLK_QUOTE		, 39).
-define(SDLK_LEFTPAREN		, 40).
-define(SDLK_RIGHTPAREN		, 41).
-define(SDLK_ASTERISK		, 42).
-define(SDLK_PLUS		, 43).
-define(SDLK_COMMA		, 44).
-define(SDLK_MINUS		, 45).
-define(SDLK_PERIOD		, 46).
-define(SDLK_SLASH		, 47).
-define(SDLK_0			, 48).
-define(SDLK_1			, 49).
-define(SDLK_2			, 50).
-define(SDLK_3			, 51).
-define(SDLK_4			, 52).
-define(SDLK_5			, 53).
-define(SDLK_6			, 54).
-define(SDLK_7			, 55).
-define(SDLK_8			, 56).
-define(SDLK_9			, 57).
-define(SDLK_COLON		, 58).
-define(SDLK_SEMICOLON		, 59).
-define(SDLK_LESS		, 60).
-define(SDLK_EQUALS		, 61).
-define(SDLK_GREATER		, 62).
-define(SDLK_QUESTION		, 63).
-define(SDLK_AT			, 64).
%	 
%%	   Skip uppercase letters
%	 
-define(SDLK_LEFTBRACKET	, 91).
-define(SDLK_BACKSLASH		, 92).
-define(SDLK_RIGHTBRACKET	, 93).
-define(SDLK_CARET		, 94).
-define(SDLK_UNDERSCORE		, 95).
-define(SDLK_BACKQUOTE		, 96).
-define(SDLK_a			, 97).
-define(SDLK_b			, 98).
-define(SDLK_c			, 99).
-define(SDLK_d			, 100).
-define(SDLK_e			, 101).
-define(SDLK_f			, 102).
-define(SDLK_g			, 103).
-define(SDLK_h			, 104).
-define(SDLK_i			, 105).
-define(SDLK_j			, 106).
-define(SDLK_k			, 107).
-define(SDLK_l			, 108).
-define(SDLK_m			, 109).
-define(SDLK_n			, 110).
-define(SDLK_o			, 111).
-define(SDLK_p			, 112).
-define(SDLK_q			, 113).
-define(SDLK_r			, 114).
-define(SDLK_s			, 115).
-define(SDLK_t			, 116).
-define(SDLK_u			, 117).
-define(SDLK_v			, 118).
-define(SDLK_w			, 119).
-define(SDLK_x			, 120).
-define(SDLK_y			, 121).
-define(SDLK_z			, 122).
-define(SDLK_DELETE		, 127).
%%	 End of ASCII mapped keysyms 

%%	 International keyboard syms 
-define(SDLK_WORLD_0		, 160).	 %%	0xA0 
-define(SDLK_WORLD_1		, 161).
-define(SDLK_WORLD_2		, 162).
-define(SDLK_WORLD_3		, 163).
-define(SDLK_WORLD_4		, 164).
-define(SDLK_WORLD_5		, 165).
-define(SDLK_WORLD_6		, 166).
-define(SDLK_WORLD_7		, 167).
-define(SDLK_WORLD_8		, 168).
-define(SDLK_WORLD_9		, 169).
-define(SDLK_WORLD_10		, 170).
-define(SDLK_WORLD_11		, 171).
-define(SDLK_WORLD_12		, 172).
-define(SDLK_WORLD_13		, 173).
-define(SDLK_WORLD_14		, 174).
-define(SDLK_WORLD_15		, 175).
-define(SDLK_WORLD_16		, 176).
-define(SDLK_WORLD_17		, 177).
-define(SDLK_WORLD_18		, 178).
-define(SDLK_WORLD_19		, 179).
-define(SDLK_WORLD_20		, 180).
-define(SDLK_WORLD_21		, 181).
-define(SDLK_WORLD_22		, 182).
-define(SDLK_WORLD_23		, 183).
-define(SDLK_WORLD_24		, 184).
-define(SDLK_WORLD_25		, 185).
-define(SDLK_WORLD_26		, 186).
-define(SDLK_WORLD_27		, 187).
-define(SDLK_WORLD_28		, 188).
-define(SDLK_WORLD_29		, 189).
-define(SDLK_WORLD_30		, 190).
-define(SDLK_WORLD_31		, 191).
-define(SDLK_WORLD_32		, 192).
-define(SDLK_WORLD_33		, 193).
-define(SDLK_WORLD_34		, 194).
-define(SDLK_WORLD_35		, 195).
-define(SDLK_WORLD_36		, 196).
-define(SDLK_WORLD_37		, 197).
-define(SDLK_WORLD_38		, 198).
-define(SDLK_WORLD_39		, 199).
-define(SDLK_WORLD_40		, 200).
-define(SDLK_WORLD_41		, 201).
-define(SDLK_WORLD_42		, 202).
-define(SDLK_WORLD_43		, 203).
-define(SDLK_WORLD_44		, 204).
-define(SDLK_WORLD_45		, 205).
-define(SDLK_WORLD_46		, 206).
-define(SDLK_WORLD_47		, 207).
-define(SDLK_WORLD_48		, 208).
-define(SDLK_WORLD_49		, 209).
-define(SDLK_WORLD_50		, 210).
-define(SDLK_WORLD_51		, 211).
-define(SDLK_WORLD_52		, 212).
-define(SDLK_WORLD_53		, 213).
-define(SDLK_WORLD_54		, 214).
-define(SDLK_WORLD_55		, 215).
-define(SDLK_WORLD_56		, 216).
-define(SDLK_WORLD_57		, 217).
-define(SDLK_WORLD_58		, 218).
-define(SDLK_WORLD_59		, 219).
-define(SDLK_WORLD_60		, 220).
-define(SDLK_WORLD_61		, 221).
-define(SDLK_WORLD_62		, 222).
-define(SDLK_WORLD_63		, 223).
-define(SDLK_WORLD_64		, 224).
-define(SDLK_WORLD_65		, 225).
-define(SDLK_WORLD_66		, 226).
-define(SDLK_WORLD_67		, 227).
-define(SDLK_WORLD_68		, 228).
-define(SDLK_WORLD_69		, 229).
-define(SDLK_WORLD_70		, 230).
-define(SDLK_WORLD_71		, 231).
-define(SDLK_WORLD_72		, 232).
-define(SDLK_WORLD_73		, 233).
-define(SDLK_WORLD_74		, 234).
-define(SDLK_WORLD_75		, 235).
-define(SDLK_WORLD_76		, 236).
-define(SDLK_WORLD_77		, 237).
-define(SDLK_WORLD_78		, 238).
-define(SDLK_WORLD_79		, 239).
-define(SDLK_WORLD_80		, 240).
-define(SDLK_WORLD_81		, 241).
-define(SDLK_WORLD_82		, 242).
-define(SDLK_WORLD_83		, 243).
-define(SDLK_WORLD_84		, 244).
-define(SDLK_WORLD_85		, 245).
-define(SDLK_WORLD_86		, 246).
-define(SDLK_WORLD_87		, 247).
-define(SDLK_WORLD_88		, 248).
-define(SDLK_WORLD_89		, 249).
-define(SDLK_WORLD_90		, 250).
-define(SDLK_WORLD_91		, 251).
-define(SDLK_WORLD_92		, 252).
-define(SDLK_WORLD_93		, 253).
-define(SDLK_WORLD_94		, 254).
-define(SDLK_WORLD_95		, 255).	%% 0xFF

%%	 Numeric keypad 
-define(SDLK_KP0		, 256).
-define(SDLK_KP1		, 257).
-define(SDLK_KP2		, 258).
-define(SDLK_KP3		, 259).
-define(SDLK_KP4		, 260).
-define(SDLK_KP5		, 261).
-define(SDLK_KP6		, 262).
-define(SDLK_KP7		, 263).
-define(SDLK_KP8		, 264).
-define(SDLK_KP9		, 265).
-define(SDLK_KP_PERIOD		, 266).
-define(SDLK_KP_DIVIDE		, 267).
-define(SDLK_KP_MULTIPLY	, 268).
-define(SDLK_KP_MINUS		, 269).
-define(SDLK_KP_PLUS		, 270).
-define(SDLK_KP_ENTER		, 271).
-define(SDLK_KP_EQUALS		, 272).

%%	 Arrows + Home/End pad 
-define(SDLK_UP			, 273).
-define(SDLK_DOWN		, 274).
-define(SDLK_RIGHT		, 275).
-define(SDLK_LEFT		, 276).
-define(SDLK_INSERT		, 277).
-define(SDLK_HOME		, 278).
-define(SDLK_END		, 279).
-define(SDLK_PAGEUP		, 280).
-define(SDLK_PAGEDOWN		, 281).

%%	 Function keys 
-define(SDLK_F1			, 282).
-define(SDLK_F2			, 283).
-define(SDLK_F3			, 284).
-define(SDLK_F4			, 285).
-define(SDLK_F5			, 286).
-define(SDLK_F6			, 287).
-define(SDLK_F7			, 288).
-define(SDLK_F8			, 289).
-define(SDLK_F9			, 290).
-define(SDLK_F10		, 291).
-define(SDLK_F11		, 292).
-define(SDLK_F12		, 293).
-define(SDLK_F13		, 294).
-define(SDLK_F14		, 295).
-define(SDLK_F15		, 296).

%%	 Key state modifier keys 
-define(SDLK_NUMLOCK		, 300).
-define(SDLK_CAPSLOCK		, 301).
-define(SDLK_SCROLLOCK		, 302).
-define(SDLK_RSHIFT		, 303).
-define(SDLK_LSHIFT		, 304).
-define(SDLK_RCTRL		, 305).
-define(SDLK_LCTRL		, 306).
-define(SDLK_RALT		, 307).
-define(SDLK_LALT		, 308).
-define(SDLK_RMETA		, 309).
-define(SDLK_LMETA		, 310).
-define(SDLK_LSUPER		, 311).	%%	 Left "Windows" key .
-define(SDLK_RSUPER		, 312).	%%	 Right "Windows" key .
-define(SDLK_MODE		, 313).	%%	 "Alt Gr" key .

%%	 Miscellaneous function keys 
-define(SDLK_HELP		, 315).
-define(SDLK_PRINT		, 316).
-define(SDLK_SYSREQ		, 317).
-define(SDLK_BREAK		, 318).
-define(SDLK_MENU		, 319).
-define(SDLK_POWER		, 320).	%%	 Power Macintosh power key .
-define(SDLK_EURO		, 321). %%	 Some european keyboards .

%%% /* Enumeration of valid key mods (possibly OR'd together) */
-define(KMOD_NONE     , 16#0000).
-define(KMOD_LSHIFT   , 16#0001).
-define(KMOD_RSHIFT   , 16#0002).
-define(KMOD_LCTRL    , 16#0040).
-define(KMOD_RCTRL    , 16#0080).
-define(KMOD_LALT     , 16#0100).
-define(KMOD_RALT     , 16#0200).
-define(KMOD_LMETA    , 16#0400).
-define(KMOD_RMETA    , 16#0800).
-define(KMOD_NUM      , 16#1000).
-define(KMOD_CAPS     , 16#2000).
-define(KMOD_MODE     , 16#4000).
-define(KMOD_RESERVED , 16#8000).
-define(KMOD_CTRL,      (?KMOD_LCTRL  bor ?KMOD_RCTRL)).
-define(KMOD_SHIFT,     (?KMOD_LSHIFT bor ?KMOD_RSHIFT)).
-define(KMOD_ALT,       (?KMOD_LALT   bor ?KMOD_RALT)).
-define(KMOD_META,      (?KMOD_LMETA  bor ?KMOD_RMETA)).

