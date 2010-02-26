%%
%%  wings_hotkey.erl --
%%
%%     This modules translates hotkeys.
%%
%%  Copyright (c) 2001-2009 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_hotkey).
-export([event/1,event/2,matching/1,bind_unicode/3,bind_virtual/4,
	 bind_from_event/2,unbind/1,hotkeys_by_commands/1,bindkey/2,
	 set_default/0,listing/0,handle_error/2]).

-define(NEED_ESDL, 1).
-include("wings.hrl").

-import(lists, [foldl/3,sort/1,foreach/2,map/2,reverse/1]).

-compile({parse_transform,ms_transform}).

-define(KL, wings_state).

%%%
%%% Hotkey lookup and translation.
%%%

event(Ev) ->
    event_1(Ev, none).

event(Ev, #st{sel=[]}) ->
    event_1(Ev, none);
event(Ev, #st{selmode=Mode}=St) ->
    case wings_light:is_any_light_selected(St) of
	true -> event_1(Ev, light);
	false -> event_1(Ev, Mode)
    end;
event(Ev, Cmd) -> event_1(Ev, Cmd).

event_1(#keyboard{}=Ev, {Selmode,_}=Cmd)
  when Selmode==vertex; Selmode==edge; Selmode==face; Selmode==body; Selmode==light ->
    case lookup(Ev, Cmd) of
	next -> event_1(Ev,Selmode);
	Action -> Action
    end;
event_1(#keyboard{}=Ev, Selmode)
  when is_atom(Selmode) ->
    case lookup(Ev, Selmode) of
	next -> lookup_1(Ev,Selmode);
	Action -> Action
    end;

event_1(#keyboard{}=Ev, Cmd) when Cmd =/= none ->
    Mode = check_for_mode_specific_menubar_items(Cmd),
    case lookup(Ev, Cmd) of
      next when is_atom(Mode) -> event_1(Ev,Mode);
      next -> lookup(Ev,none);
      Action -> Action
    end;
event_1(_, _) -> next.

lookup(Ev, Cmd) ->
    case ets:lookup(?KL, bindkey(Ev, Cmd)) of
	[{_,Action,_}] -> Action;
	[] -> next
    end.

lookup_1(Ev,SelMode) ->
%% Checks for menubar items that are selection mode specific called by hotkey.
    case lookup(Ev, none) of
      {select,{edge_loop,edge_loop}}=EL when SelMode == face ->
          EL;
      {select,{edge_loop,complete_loops}}=EL ->
          EL;
      {select,{edge_loop,_}} when SelMode =/= edge ->
          next;
      {select,{oriented_faces,_}} when SelMode =/= face ->
          next;
      {select,{similar_material,_}} when SelMode =/= face ->
          next;
      {select,{similar_area,_}} when SelMode =/= face ->
          next;
      {tools,{virtual_mirror,create}} when SelMode =/= face ->
          next;
      Other -> Other
    end.

check_for_mode_specific_menubar_items({select,{edge_loop,_}}) -> edge;
check_for_mode_specific_menubar_items({select,{oriented_faces,_}}) -> face;
check_for_mode_specific_menubar_items({select,{similar_material,_}}) -> face;
check_for_mode_specific_menubar_items({select,{similar_area,_}}) -> face;
check_for_mode_specific_menubar_items({tools,{virtual_mirror,create}}) -> face;
check_for_mode_specific_menubar_items(Cmd) -> Cmd.

%%%
%%% Binding and unbinding of keys.
%%%

bind_from_event(Ev, Cmd) ->
    Bkey = bindkey(Ev, Cmd),
    ets:insert(?KL, {Bkey,Cmd,user}),
    keyname(Bkey).


unbind(Key) ->
    ets:delete(?KL, Key).

hotkeys_by_commands(Cs) ->
    hotkeys_by_commands_1(Cs, []).

hotkeys_by_commands_1([C|Cs], Acc) ->
    Ms = ets:match_object(?KL, {'_',C,'_'}),
    hotkeys_by_commands_1(Cs, Ms++Acc);
hotkeys_by_commands_1([], Acc) ->
    hotkeys_by_commands_2(sort(Acc)).

hotkeys_by_commands_2([{Key,Cmd,Src}|T]) ->
    Info = {Key,keyname(Key),wings_util:stringify(Cmd),Src},
    [Info|hotkeys_by_commands_2(T)];
hotkeys_by_commands_2([]) -> [].
    
bind_unicode(Key, Cmd, Source) ->
    Bkey = bkey(Key, Cmd),
    ets:insert(?KL, {Bkey,Cmd,Source}),
    Bkey.

bind_virtual(Key, Mods, Cmd, Source) ->
    Bkey = bkey({Key,sort(Mods)}, Cmd),
    ets:insert(?KL, {Bkey,Cmd,Source}),
    Bkey.

bindkey(#keyboard{sym=?SDLK_TAB=C,mod=Mod}, Cmd) ->
    bkey({C,sort(modifiers(Mod))}, Cmd);
bindkey(#keyboard{sym=Sym,mod=Mod,unicode=C}, Cmd) ->
    case modifiers(Mod) of
	[] when C =/= 0 ->
	    bkey(fix_bksp_and_del(Sym, C), Cmd);
	[shift] when C =/= 0 ->
	    bkey(C, Cmd);
	Mods ->
	    bkey({Sym,sort(Mods)}, Cmd)
    end.

bkey(Key, {Mode,_}) ->
    bkey(Key, Mode);
bkey(Key, Mode) ->
    case suitable_mode(Mode) of
	true -> {bindkey,Mode,Key};
	false -> {bindkey,Key}
    end.
    
matching(Names) ->
    M0 = matching_global(Names) ++ matching_mode(Names),
    M = wings_util:rel2fam(M0),
    [{Name,Key} || {Name,[{_,Key}|_]} <- M].

matching_global(Names) ->
    Spec0 = foldl(fun(N, A) -> {N,A} end, '$1', Names),
    Spec = [{{{bindkey,'$2'},Spec0,'$3'},
	     [],
	     [{{'$1',{{'$3','$2'}}}}]}],
    [{Name,mkeyname(Key)} || {Name,Key} <- ets:select(?KL, Spec)].

matching_mode(Names) ->
    Mode = lists:last(Names),
    case suitable_mode(Mode) of
	false -> [];
	true ->
	    Spec0 = foldl(fun(N, A) -> {N,A} end, '$1', Names),
	    Spec = [{{{bindkey,Mode,'$2'},Spec0,'$3'},
		     [],
		     [{{'$1',{{'$3','$2'}}}}]}],
	    [{Name,mkeyname(Key)} || {Name,Key} <- ets:select(?KL, Spec)]
    end.

mkeyname({user,K}) -> {1,keyname(K)};
mkeyname({default,K}) -> {2,keyname(K)};
mkeyname({plugin,K}) -> {3,keyname(K)}.

suitable_mode(light) -> true;
suitable_mode(vertex) -> true;
suitable_mode(edge) -> true;
suitable_mode(face) -> true;
suitable_mode(body) -> true;
suitable_mode(_) -> false.

%%%
%%% Make a listing of all hotkeys.
%%%

listing() ->
    MatchSpec = ets:fun2ms(fun({{bindkey,K},Cmd,Src}) ->
				   {all,{{bindkey,K},Cmd,Src}};
			      ({{bindkey,Mode,K},Cmd,Src}) ->
				   {Mode,{{bindkey,K},Cmd,Src}}
			   end),
    Keys = wings_util:rel2fam(ets:select(?KL, MatchSpec)),
    listing_1(Keys, []).

listing_1([{Mode,Keys}|T], Acc0) ->
    Acc = [list_keys(Keys),list_header(Mode)|Acc0],
    listing_1(T, Acc);
listing_1([], Acc) -> reverse(Acc).

list_header(all) ->?STR(list_header,1,"Hotkeys in all modes");
list_header(body) ->?STR(list_header,2,"Hotkeys in object mode");
list_header(edge) -> ?STR(list_header,3,"Hotkeys in edge mode");
list_header(face) -> ?STR(list_header,4,"Hotkeys in face mode");
list_header(light) -> ?STR(list_header,5,"Hotkeys for lights");
list_header(vertex) -> ?STR(list_header,6,"Hotkeys for vertices");
list_header(A) -> atom_to_list(A).

list_keys([{Key,Cmd,Src}|T]) ->
    KeyStr = keyname(Key),
    SrcStr = case Src of
		 default -> "";
		 user -> ?STR(list_keys,1," (user-defined)");
		 plugin -> ?STR(list_keys,2," (plug-in-defined)")
	     end,
    KeyStr ++ ": " ++ wings_util:stringify(Cmd) ++ SrcStr ++ 
	"\n" ++ list_keys(T);
list_keys([]) -> [].


%%%
%%% Error handling.
%%%
handle_error(Ev, Cmd) ->
    Key = bindkey(Ev, Cmd),
    KeyName = keyname(Key),
    CmdStr = wings_util:stringify(Cmd),
    Msg1 = "Executing the command \"" ++ CmdStr ++ "\"\nbound to the hotkey " ++
	KeyName ++ " caused an error.",
    Msg2 = "Possible causes:",
    Msg3 = [bullet,$\s|"The hotkey was defined in a previous version of Wings,\n"
	"and the command that it refers to has been changed,\n"
	"removed, or renamed in this version of Wings."],
    Msg4 = [bullet,$\s|"The hotkey refers to a command in a "
	    " plug-in that is currently disabled,\n"
	    "or to a previous version of a plug-in."],
    Msg5 = [bullet,$\s|"A bug in the command itself. Try executing the command\n"
	    "from the menu directly (i.e. not through a hotkey) -\n"
	    "if it crashes it IS a bug. (Please report it.)"],
    Qs = {vframe,
	  [{label,Msg1},{panel,[]},
	   {label,Msg2},
	   {label,Msg3},
	   {label,Msg4},
	   {label,Msg5},{panel,[]},
	   {hframe,[{button,"Ignore",fun(_) -> ignore end,
		     [{info,"Do nothing"}]},
		    {button,"Delete Hotkey",fun(_) -> unbind(Key) end,
		     [{info,"Delete the hotkey "++KeyName}]}]}]},
    wings_ask:dialog("", Qs, fun(_) -> ignore end).

%%%
%%% Local functions.
%%%

%% For the benefit of Mac OS X, but does no harm on other platforms.
fix_bksp_and_del(?SDLK_DELETE, _) -> ?SDLK_DELETE;
fix_bksp_and_del(?SDLK_BACKSPACE, _) -> ?SDLK_BACKSPACE;
fix_bksp_and_del(_, C) -> C.

modifiers(Mod) ->
    modifiers(Mod, []).
modifiers(Mod, Acc) when Mod band ?CTRL_BITS =/= 0 ->
    Pressed = Mod band ?CTRL_BITS,
    modifiers(Mod bxor Pressed, [ctrl|Acc]);
modifiers(Mod, Acc) when Mod band ?ALT_BITS =/= 0 ->
    Pressed = Mod band ?ALT_BITS,
    modifiers(Mod bxor Pressed, [alt|Acc]);
modifiers(Mod, Acc) when Mod band ?SHIFT_BITS =/= 0 ->
    Pressed = Mod band ?SHIFT_BITS,
    modifiers(Mod bxor Pressed, [shift|Acc]);
modifiers(Mod, Acc) when Mod band ?KMOD_META =/= 0 ->
    Pressed = Mod band ?KMOD_META,
    modifiers(Mod bxor Pressed, [command|Acc]);
modifiers(_, Acc) -> lists:sort(Acc).

keyname({bindkey,Key}) ->
    keyname(Key);
keyname({bindkey,_Mode,Key}) ->
    keyname(Key);
keyname({C,Mods}) ->
    modname(Mods) ++ vkeyname(C);
keyname($\b) -> ?STR(keyname,1,"Bksp");
keyname($\t) -> ?STR(keyname,2,"Tab");
keyname($\s) -> ?STR(keyname,3,"Space");
keyname(C) when $a =< C, C =< $z -> [C-32];
keyname(C) when $A =< C, C =< $Z ->
    case get(wings_os_type) of
	{unix,darwin} -> [shift,C];
	_ -> ?STR(keyname,4,"Shift+")++ [C]
    end;
keyname(C) when is_integer(C), C < 256 -> [C];
keyname(C) when is_integer(C), 63236 =< C, C =< 63247 ->
    [$F|integer_to_list(C-63235)];
keyname(C) -> [$<|integer_to_list(C)++">"].

modname(Mods) ->
    case get(wings_os_type) of
	{unix,darwin} -> mac_modname(Mods, []);
	_ -> modname_1(Mods)
    end.

modname_1([command|T]) -> "Meta+" ++modname_1(T);
modname_1([Mod|T]) -> wings_s:modkey(Mod) ++ "+" ++modname_1(T);
modname_1([]) -> [].

mac_modname([ctrl|T], Acc) -> [$^|mac_modname(T, Acc)];
mac_modname([shift|T], Acc) -> mac_modname(T, [shift|Acc]);
mac_modname([alt|T], Acc) -> mac_modname(T, [option|Acc]);
mac_modname([command|T], Acc) -> mac_modname(T, Acc++[command]);
mac_modname([], Acc) -> Acc.

vkeyname(?SDLK_BACKSPACE) -> ?STR(vkeyname,1,"Bksp");
vkeyname(?SDLK_TAB) -> ?STR(vkeyname,2,"Tab");
vkeyname(?SDLK_RETURN) -> ?STR(vkeyname,3,"Enter");
vkeyname(?SDLK_PAUSE) -> ?STR(vkeyname,4,"Pause");
vkeyname(?SDLK_ESCAPE) ->?STR(vkeyname,5,"Esc");
vkeyname(?SDLK_SPACE) -> ?STR(vkeyname,6,"Space");
vkeyname(?SDLK_DELETE) -> ?STR(vkeyname,7,"Delete");
vkeyname(C) when $a =< C, C =< $z-> [C-32];
vkeyname(C) when $\s < C, C < 256 -> [C];
vkeyname(C) when ?SDLK_KP0 < C, C < ?SDLK_KP9 -> [C-?SDLK_KP0+$0];
vkeyname(C) when ?SDLK_F1 =< C, C =< ?SDLK_F15 ->
    [$F|integer_to_list(C-?SDLK_F1+1)];
vkeyname(?SDLK_KP_PERIOD) -> ?STR(vkeyname,8,"Del");
vkeyname(?SDLK_KP_DIVIDE) -> ?STR(vkeyname,9,"Div");
vkeyname(?SDLK_KP_MULTIPLY) -> ?STR(vkeyname,10,"Mul");
vkeyname(?SDLK_KP_MINUS) -> ?STR(vkeyname,11,"-");
vkeyname(?SDLK_KP_PLUS) -> ?STR(vkeyname,12,"+");
vkeyname(?SDLK_KP_ENTER) -> ?STR(vkeyname,13,"Enter");
vkeyname(?SDLK_KP_EQUALS) ->?STR(vkeyname,14,"=");
vkeyname(?SDLK_UP) -> ?STR(vkeyname,15,"Up");
vkeyname(?SDLK_DOWN) -> ?STR(vkeyname,16,"Down");
vkeyname(?SDLK_RIGHT) -> ?STR(vkeyname,17,"Right");
vkeyname(?SDLK_LEFT) -> ?STR(vkeyname,18,"Left");
vkeyname(?SDLK_INSERT) -> ?STR(vkeyname,19,"Insert");
vkeyname(?SDLK_HOME) -> ?STR(vkeyname,20,"Home");
vkeyname(?SDLK_END) -> ?STR(vkeyname,21,"End");
vkeyname(?SDLK_PAGEUP) -> ?STR(vkeyname,22,"Page Up");
vkeyname(?SDLK_PAGEDOWN) ->?STR(vkeyname,23,"Page Down");
vkeyname(_) -> ?STR(vkeyname,24,"UKEY").

%%%
%%% Default keybindings.
%%%

set_default() ->
    foreach(
      fun({{Key,List0},Action}) when is_integer(Key) ->
	      List = convert_modifiers(List0),
	      ets:insert(wings_state, {{bindkey,{Key,sort(List)}},
				       Action,default});
	 ({Key,Action}) when is_integer(Key) ->
	      ets:insert(wings_state, {{bindkey,Key},
				       Action,default});
	 ({Mode,{Key,List0},Action}) when is_integer(Key) ->
	      List = convert_modifiers(List0),
	      ets:insert(wings_state, {{bindkey,Mode,{Key,sort(List)}},
				       Action,default});
	 ({Mode,Key,Action}) when is_integer(Key) ->
	      ets:insert(wings_state, {{bindkey,Mode,Key},
				       Action,default})
      end, default_keybindings()).

convert_modifiers(Mod) ->
    case get(wings_os_type) of
	{unix,darwin} ->
	    map(fun(ctrl) -> command;
		   (Other) -> Other end, Mod);
	_ -> Mod
    end.

default_keybindings() ->
    [{{$a,[ctrl]},          {select,all}},
     {{$i,[ctrl,shift]},    {select,inverse}},
     {{$l,[ctrl]},          {file,merge}},
     {{$n,[ctrl]},          {file,new}},
     {{$o,[ctrl]},          {file,open}},
     {{$q,[ctrl]},          {file,quit}},
     {{$s,[ctrl,shift]},    {file,save_as}},
     {{$s,[ctrl]},          {file,save}},
     {{$z,[alt,ctrl]},      {edit,undo}},
     {{$z,[ctrl,shift]},    {edit,redo}},
     {{$z,[ctrl]},          {edit,undo_toggle}},
     {{?SDLK_KP_PLUS,[]},   {select,more}},
     {{?SDLK_KP_MINUS,[]},  {select,less}},
     {{?SDLK_F1,[]},        {tweak,{axis_constraint,x}}},
     {{?SDLK_F2,[]},        {tweak,{axis_constraint,y}}},
     {{?SDLK_F3,[]},        {tweak,{axis_constraint,z}}},
     {{?SDLK_F6,[]},        {select,{edge_loop,prev_edge_loop}}},
     {{?SDLK_F7,[]},        {select,{edge_loop,next_edge_loop}}},
     {{?SDLK_F5,[]},        {select,{by,{faces_with,5}}}},
     {{?SDLK_TAB,[]},       {view,workmode}},
     {{?SDLK_TAB,[shift]},  {view,quick_preview}},
     {$\s,              {select,deselect}},
     {$a,               {view,highlight_aim}},
     {$A,               {view,frame}},
     {$b,               {select,body}},
     {{$d,[ctrl]},      {edit,repeat}},
     {$d,               {edit,repeat_args}},
     {$D,               {edit,repeat_drag}},
     {$e,               {select,edge}},

     {$f,               {select,face}},
     {$g,               {select,{edge_loop,edge_ring}}},
     {{$g,[alt]},       {select,{edge_loop,edge_ring_incr}}},
     {{$g,[alt,ctrl]},  {select,{edge_loop,edge_ring_decr}}},
     {$i,               {select,similar}},
     {$l,               {select,{edge_loop,edge_loop}}},
     {$L,		{select,{edge_loop,edge_loop_to_region}}},
     {{$l,[alt]},       {select,{edge_loop,edge_link_incr}}},
     {{$l,[alt,ctrl]},  {select,{edge_loop,edge_link_decr}}},
     {$o,               {view,orthogonal_view}},
     {$r,               {view,reset}},
     {$u,               {view,auto_rotate}},
     {$v,               {select,vertex}},
     {$w,               {view,toggle_wireframe}},
     {$x,               {view,{along,x}}},
     {$y,               {view,{along,y}}},
     {$z,               {view,{along,z}}},
     {$X,               {view,{along,neg_x}}},
     {$Y,               {view,{along,neg_y}}},
     {$Z,               {view,{along,neg_z}}},
     {$+,               {select,more}},
     {$=,               {select,more}},
     {$-,               {select,less}},

     %% Mode-specific bindings.
     {edge,$2,		{edge,{cut,2}}},
     {edge,$3,		{edge,{cut,3}}},
     {edge,$4,		{edge,{cut,4}}},
     {edge,$5,		{edge,{cut,5}}},
     {edge,$6,		{edge,{cut,6}}},
     {edge,$7,		{edge,{cut,7}}},
     {edge,$8,		{edge,{cut,8}}},
     {edge,$9,		{edge,{cut,9}}},
     {edge,$0,		{edge,{cut,10}}},

     {vertex,$c,	{vertex,connect}},
     {edge,$c,		{edge,connect}},
      
     {vertex,$\b,	{vertex,collapse}},
     {edge,$\b,		{edge,dissolve}},
     {face,$\b,		{face,dissolve}},
     {body,$\b,		{body,delete}},

     {vertex,{?SDLK_DELETE,[]},	{vertex,dissolve}},
     {edge,{?SDLK_DELETE,[]},	{edge,dissolve}},
     {face,{?SDLK_DELETE,[]},	{face,dissolve}},
     {body,{?SDLK_DELETE,[]},	{body,delete}},

     {vertex,{?SDLK_KP_PERIOD,[]},{vertex,dissolve}},
     {edge,{?SDLK_KP_PERIOD,[]},  {edge,dissolve}},
     {face,{?SDLK_KP_PERIOD,[]},  {face,dissolve}},
     {body,{?SDLK_KP_PERIOD,[]},  {body,delete}},

     {light,$\b,	{light,delete}},
     {light,{?SDLK_DELETE,[]},	{light,delete}},
     {light,{?SDLK_KP_PERIOD,[]},  {light,delete}},

     {face,$s,		{face,smooth}},
     {body,$s,		{body,smooth}}
    ].
