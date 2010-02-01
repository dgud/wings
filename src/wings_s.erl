%%
%%  wings_s.erl --
%%
%%     Common text strings.
%%
%%  Copyright (c) 2004-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_s).
-export([yes/0,no/0,cancel/0,accept/0,
	 lmb/0,mmb/0,rmb/0,scroll/0,
	 modkey/1,shift/0,ctrl/0,alt/0,command/0,
	 key/1,dir/1,dir_axis/1,
	 camera_mode/1]).

-include("wings.hrl").

yes() -> ?__(1,"Yes").
no() -> ?__(1,"No").
cancel() -> ?__(1,"Cancel").
accept() -> ?__(1,"Accept").
    
%% Mouse buttons.
lmb() -> ?STR(mouse_b,l,"L").
mmb() -> ?STR(mouse_b,m,"M").
rmb() -> ?STR(mouse_b,r,"R").
scroll() -> ?__(scroll,"Scroll").

%% Modifier keys.
shift() ->   ?STR(mod,shift,"Shift").
ctrl() ->    ?STR(mod,ctrl,"Ctrl").
alt() ->     ?STR(mod,alt,"Alt").
command() -> ?STR(mod,command,"Command").		%Command key on Mac.

modkey(shift) -> shift();
modkey(ctrl) -> ctrl();
modkey(alt) -> alt();
modkey(command) -> command().

%% Returns key name within square brackets.
key(Key) -> [$[,key_1(Key),$]].

key_1(shift) -> shift();
key_1(ctrl) -> ctrl();
key_1(alt) -> alt();
key_1(command) -> command();
key_1(Key) when is_atom(Key) -> atom_to_list(Key);
key_1(Key) when is_list(Key) -> Key.

%% All directions.        
dir(x) -> ?__(x,"X");
dir(y) -> ?__(y,"Y");
dir(z) -> ?__(z,"Z");
dir(all) -> ?__(all,"All");
dir(last_axis) -> ?__(la,"last axis");
dir(default_axis) -> ?__(da,"default axis");
dir(normal) -> ?__(n,"Normal");
dir(free) ->  ?__(f,"Free");
dir(uniform) ->  ?__(u,"Uniform");
dir({radial,Axis}) ->  ?__(r,"Radial") ++ " " ++ dir(Axis);
dir(radial_x) ->  ?__(r,"Radial") ++ " " ++ dir(x);
dir(radial_y) ->  ?__(r,"Radial") ++ " " ++ dir(y);
dir(radial_z) ->  ?__(r,"Radial") ++ " " ++ dir(z).

dir_axis(Axis) -> 
    wings_util:format(?STR(dir,the_axis,"the ~s axis"), [dir(Axis)]).

%% Camera modes; probably don't need to be translated, but could
%% need to be transliterad for languages with non-Latin alphabets.
camera_mode(blender) -> ?__(blender,"Blender");
camera_mode(nendo) -> ?__(nendo,"Nendo");
camera_mode(mirai) -> ?__(mirai,"Mirai");
camera_mode(tds) -> ?__(tds,"3ds max");
camera_mode(maya) -> ?__(maya,"Maya");
camera_mode(mb) -> ?__(mb,"Motionbuilder");
camera_mode(sketchup) -> ?__(sketchup,"SketchUp");
camera_mode(wings_cam) -> ?__(wings_cam,"Wings 3D").
