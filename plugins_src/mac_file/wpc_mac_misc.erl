%%
%%  wpc_mac_misc.erl
%%
%%     Miscellanous plug-in stuff for Mac.
%%
%%  Copyright (c) 2004-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_mac_misc).

-export([init/0,menu/2,command/2]).

-include("wings_intl.hrl").

init() ->
    wpa:pref_set_default(?MODULE, native_file_dialog, true),
    true.

menu({file}, Menu) ->
    add_our_stuff(Menu);
menu(_, Menu) -> Menu.

add_our_stuff([{_,install_plugin,_}|_]=Menu) ->
    %% Preferred location.
    mac_stuff([separator|Menu]);
add_our_stuff([{_,quit}|_]=Menu) ->
    %% Fallback location.
    mac_stuff([separator|Menu]);
add_our_stuff([H|T]) ->
    [H|add_our_stuff(T)];
add_our_stuff([]) ->
    %% Final fallback location - at the very end.
    [separator|mac_stuff([])].

mac_stuff(Menu) ->
    [{?__(1,"Mac OS File Dialog"),native_file_dialog,
      ?__(2,"Choose whether to use the Mac OS standard file dialog or Wings' own file dialog"),
      case wpa:pref_get(?MODULE, native_file_dialog) of
	  false -> [{crossmark, false}];
	  true -> [{crossmark, true}]
      end}|Menu].

command({file,native_file_dialog}, St) ->
    wpa:pref_set(?MODULE, native_file_dialog,
		 not wpa:pref_get(?MODULE, native_file_dialog)),
    St;
command(_, _) -> next.
