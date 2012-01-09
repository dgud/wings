%%
%%  wings_msg.erl --
%%
%%     Helpers for formatting and showing messages.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_msg).

-export([button/1,button/2,button/3,
	 button_format/1,button_format/2,button_format/3,
	 rmb_format/1,join/1,join/2,
	 free_modifier/0,free_lmb_modifier/0,additional_free_lmb_modifier/0,
	 free_rmb_modifier/0,mod_name/1,mod_format/3]).

-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [reverse/1]).
-define(SEP, [160,160,160,160,160]).    %Equivalent to two and a half space character width.
-define(CSEP, 160).				%Short space.

button(LmbMsg) ->
    wings_wm:message(button_format(LmbMsg)).

button(LmbMsg, MmbMsg) ->
    wings_wm:message(button_format(LmbMsg, MmbMsg)).

button(LmbMsg, MmbMsg, RmbMsg) ->
    wings_wm:message(button_format(LmbMsg, MmbMsg, RmbMsg)).

button_format(LmbMsg) ->
    [lmb_name(),?CSEP|LmbMsg].

button_format(LmbMsg, MmbMsg) ->
    button_format(LmbMsg, MmbMsg, []).
    
button_format(Msg, [], Msg) when Msg =/= [] ->
    Buttons = wings_pref:get_value(num_buttons),
    Lmb = drop_last(lmb_name()),
    Rmb = rmb_name(Buttons),
    [Lmb,$,,Rmb,?CSEP|Msg];
button_format(LmbMsg, Msg, Msg) when Msg =/= [] ->
    Buttons = wings_pref:get_value(num_buttons),
    Lmb = lmb_name(),
    Mmb = drop_last(mmb_name(Buttons)),
    Rmb = rmb_name(Buttons),
    Lmsg = if
	       LmbMsg =/= [] -> [Lmb,?CSEP|LmbMsg];
	       true -> []
	   end,
    RMmsg = [Mmb,$,,Rmb,?CSEP|Msg],
    join(Lmsg, RMmsg);
button_format(LmbMsg, MmbMsg, RmbMsg) ->
    Buttons = wings_pref:get_value(num_buttons),
    Lmb = lmb_name(),
    Mmb = mmb_name(Buttons),
    Rmb = rmb_name(Buttons),
    Lmsg = if
	       LmbMsg =/= [] -> [Lmb,?CSEP|LmbMsg];
	       true -> []
	   end,
    Mmsg = if
	       MmbMsg =/= [] -> [Mmb,?CSEP|MmbMsg];
	       true -> []
	   end,
    Rmsg = if
	       RmbMsg =/= [] -> [Rmb,?CSEP|RmbMsg];
	       true -> []
	   end,
    join(Lmsg, join(Mmsg, Rmsg)).

join([M0,M1|T]) ->
    join(join(M0, M1), join(T));
join([M]) -> M;
join([]) -> [].

join([], Msg) -> Msg;
join(Msg, []) -> Msg;
join(Msg1, Msg2) -> [Msg1,?SEP,Msg2].

rmb_format(Message) ->
    ModName = mod_name(free_rmb_modifier()),
    [ModName,$+,rmb_name(),?CSEP|Message].

free_modifier() ->
    case wings_pref:get_value(num_buttons) of
	1 -> ?META_BITS;
	_ -> ?CTRL_BITS
    end.

free_lmb_modifier() ->
    case wings_pref:get_value(camera_mode) of
	maya -> ?CTRL_BITS;
	blender -> ?CTRL_BITS;
	nendo ->
	    case wings_pref:get_value(num_buttons) of
		1 -> ?META_BITS;
		_ -> ?ALT_BITS
	    end;
	_ -> ?ALT_BITS
    end.

additional_free_lmb_modifier() ->
    case wings_pref:get_value(camera_mode) of
	mb -> none;
	_ -> ?SHIFT_BITS
    end.

free_rmb_modifier() ->
    case wings_pref:get_value(camera_mode) of
	maya -> ?CTRL_BITS;
	nendo ->
	    case wings_pref:get_value(num_buttons) of
		1 -> ?META_BITS;
		_ -> ?ALT_BITS
	    end;
	_ -> ?ALT_BITS
    end.

mod_format(Mod, 1, Msg) ->
    mod_format(Mod, lmb_name(), Msg);
mod_format(Mod, 2, Msg) ->
    mod_format(Mod, mmb_name(), Msg);
mod_format(Mod, 3, Msg) ->
    mod_format(Mod, rmb_name(), Msg);
mod_format(0, But, Msg) ->
    [But,?CSEP,Msg];
mod_format(Mod, But, Msg) ->
    M0 = [But,?CSEP,Msg],
    M1 = if
	     (Mod band ?SHIFT_BITS) =/= 0 ->
		 [wings_s:key(shift),$+|M0];
	     true -> M0
	 end,
    M2 = if
	     (Mod band ?ALT_BITS) =/= 0 ->
		 [wings_s:key(alt),$+|M1];
	     true -> M1
	 end,
    M3 = if
	     (Mod band ?CTRL_BITS) =/= 0 ->
		 [wings_s:key(ctrl),$+|M2];
	     true -> M2
	 end,
    if
	(Mod band ?META_BITS) =/= 0 ->
	    [wings_s:key(command),$+|M3];
	true -> M3
    end.

mod_name(?ALT_BITS) -> wings_s:key(alt);
mod_name(?CTRL_BITS) -> wings_s:key(ctrl);
mod_name(?META_BITS) -> wings_s:key(command).

%%%
%%% Local functions.
%%%

drop_last(S) ->
    reverse(tl(reverse(S))).

lmb_name() -> [wings_s:lmb()|":"].

mmb_name() ->
    mmb_name(wings_pref:get_value(num_buttons)).

mmb_name(3) -> [wings_s:mmb()|":"];
mmb_name(2) ->
    case wings_pref:get_value(camera_mode) of
	blender -> [wings_s:key(alt),$+|lmb_name()];
	nendo -> [wings_s:key(ctrl),$+|rmb_name(2)]
    end;
mmb_name(1) -> [wings_s:key(alt),$+|lmb_name()].

rmb_name() ->
    rmb_name(wings_pref:get_value(num_buttons)).

rmb_name(1) -> [wings_s:key(ctrl),$+,lmb_name()];
rmb_name(_) -> [wings_s:rmb()|":"].
