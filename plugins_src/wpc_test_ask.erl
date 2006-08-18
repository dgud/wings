%%
%%  wpc_test_ask.erl --
%%
%%     Default disabled test plugin for dialogs (wings_ask).
%%
%%  Copyright (c) 2003-2004 Raimo Niskanen
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_test_ask.erl,v 1.32 2004/12/18 19:35:58 bjorng Exp $
%%

-module(wpc_test_ask).

-include_lib("kernel/include/file.hrl").

-export([enable/0,disable/0]).
-export([load/1]).
-export([init/0,menu/2,dialog/2,command/2]).

-import(lists, [reverse/1,reverse/2]).

enable() -> wpa:pref_set(?MODULE, enabled, true).

disable() -> wpa:pref_delete(?MODULE, enabled).

enabled() -> wpa:pref_get(?MODULE, enabled).
    
load(File) ->
    load(wings_ask, File).

load(Mod, File) ->
    Ext = code:objfile_extension(),
    Filename = filename:rootname(File, Ext) ++ Ext,
    {ok,Code} = file:read_file(Filename),
    code:purge(Mod),
    code:load_binary(Mod, Filename, Code).


-define(DISPLAY(X), io:format(?MODULE_STRING":~w ~p~n", [?LINE,X])).

init() ->
    case enabled() of
	true -> true;
	_ -> false
    end.

menu({tools}, Menu) ->
    case enabled() of
	true -> 
	    Menu++
		[separator,
		 {"Test Ask",{?MODULE,[{"Minimal Dialog",minimal},
				       {"Large Dialog",large},
				       {"Overlay Dialog",overlay},
				       {"Dynamic Dialog",dynamic},
				       {"Disabled Frames Dialog",
					disabled_frames},
				       separator,
				       {"Table Dialog",table},
				       {"Filename Dialog",filename},
				       {"Open Dialog",open_dialog}]}}];
	_ -> Menu
    end;
menu(_, Menu) -> Menu.

command({tools,{?MODULE,minimal}}, St) ->
    maybe_dialog(fun minimal_dialog/1, St);
command({tools,{?MODULE,large}}, St) ->
    maybe_dialog(fun large_dialog/1, St);
command({tools,{?MODULE,overlay}}, St) ->
    maybe_dialog(fun overlay_dialog/1, St);
command({tools,{?MODULE,dynamic}}, St) ->
    maybe_dialog(fun dynamic_dialog/1, St);
command({tools,{?MODULE,disabled_frames}}, St) ->
    maybe_dialog(fun disabled_frames/1, St);
command({tools,{?MODULE,table}}, St) ->
    maybe_dialog(fun table_dialog/1, St);
command({tools,{?MODULE,filename}}, St) ->
    maybe_dialog(fun filename_dialog/1, St);
command({tools,{?MODULE,open_dialog}}, St) ->
    maybe_dialog(fun open_dialog/1, St);
command(_, _St) ->
    next.

maybe_dialog(Dialog, St) ->
    case enabled() of
	true -> Dialog(St);
	_ -> next
    end.

dialog({material_editor_setup,_Name,_Mat}, Dialog) ->
    case enabled() of true -> Dialog++[{"Test Ask",true}];
	_ -> Dialog
    end;
dialog({material_editor_result,_Name,Mat}, [X|R]=Res) ->
    case enabled() of true -> 
	    ?DISPLAY(X),
	    {Mat,R};
	_ -> {Mat,Res}
    end;
dialog({light_editor_setup,_Name,_Ps}, Dialog) ->
    case enabled() of true -> Dialog++[{"Foo",true}];
	_ -> Dialog
    end;
dialog({light_editor_result,_Name,Ps}, [X|R]=Res) ->
    case enabled() of true ->
	    ?DISPLAY(X),
	    {Ps,R};
	_ -> {Ps,Res}
    end;
dialog(_X, Dialog) ->
    io:format("~p\n", [{_X,Dialog}]),
    Dialog.

table_dialog(_) ->
    Dir = filename:dirname(code:which(wings)),
    {ok,Files0} = file:list_dir(Dir),
    Files = [table_file(F, Dir) || F <- Files0],
    Qs = [{table,[{"Filename","Size","Modified"}|Files]}],
    Ask = fun(Res) ->
		  io:format("~p\n", [Res]),
		  ignore
	  end,
    wings_ask:dialog("", Qs, Ask).

table_file(F, Dir) ->
    {ok,#file_info{mtime=Mtime,size=Sz}} =
	file:read_file_info(filename:join(Dir, F)),
    DateTime = lists:flatten(io_lib:format("~p", [Mtime])),
    {{F,F},{Sz,integer_to_list(Sz)},{Mtime,DateTime}}.

filename_dialog(_) ->
    Filename = code:which(?MODULE),
    {dialog,Qs,Ask} = do_filename_dialog(Filename),
    wings_ask:dialog("", Qs, Ask).

do_filename_dialog(Filename) ->
    Ps = [{dialog_type,open_dialog},{ext,".beam"},{ext_desc,"Beam File"}],
    Qs = [{hframe,
	   [{label,"Filename"},
	    {button,{text,Filename,[{props,Ps}]}}]}],
    Ask = fun(Res) ->
		  io:format("~p\n", [Res]),
		  ignore
	  end,
    {dialog,Qs,Ask}.

open_dialog(_) ->
    F = fun(Res) ->
		io:format("~p\n", [Res]),
		ignore
	end,
    Dir = wings_pref:get_value(current_directory),
    Ps = [{directory,Dir},{ext,".wings"},{ext_desc,"Wings File"}],
    wings_plugin:call_ui({file,open_dialog,Ps,F}).


minimal_dialog(_St) ->
    Fun = fun(Res) -> 
		  ?DISPLAY(Res),
		  case Res of 
		      [{a,true}|_] -> 
			  wpa:error("Uncheck the checkbox!");
		      _ -> ok
		  end, ignore
	  end,
    Dialog =
	[{hframe,[{label,"Label"},{"Checkbox",false,[{key,a}]}],
	 [{title,"Hframe"}]}],
    wings_ask:dialog("Test Ask Minimal", Dialog, Fun).



large_dialog(St) -> 
    wings_ask:dialog("Test Ask Large",
		     mk_large_dialog(false, true, false),
		     large_result(St)).

mk_large_dialog(MinimizedL, MinimizedC, MinimizedR) ->
    [{hframe,[large_dialog_l(MinimizedL, MinimizedC),
	      large_dialog_r(MinimizedR)]}].

large_result(St) ->
    fun ([MinimizedL|Res]) -> 
	    ?DISPLAY(Res),
	    MinimizedC = proplists:get_value(minimized_c, Res),
	    MinimizedR = proplists:get_value(minimized_r, Res),
	    case proplists:get_value(reset, Res) of
		true -> 
		    {dialog,
		     mk_large_dialog(MinimizedL, MinimizedC, MinimizedR),
		     large_result(St)};
		false -> ignore
	    end
    end.

overlay_dialog(St) ->
    wings_ask:dialog("Test Ask Overlay", 
		     mk_overlay_dialog(buttons, 1, false, true, false), 
		     overlay_result(St)).

mk_overlay_dialog(Style, Active, MinimizedL, MinimizedC, MinimizedR) ->
    Dialog =
	[{hframe,[{label,"Style  "},
		  {hradio,[{"Buttons",buttons},{"Menu",menu}],Style,
		   [{hook,fun (update, {Var,_I,Val,Sto}) ->
				  ?DISPLAY([update,{Var,_I,Val,sto}]),
				  {done,gb_trees:update(Var, Val, Sto)};
			      (_, _) -> void end}]}]},
	 {oframe,
	  [{"Left frame",large_dialog_l(MinimizedL, MinimizedC)},
	   {"Right frame",large_dialog_r(MinimizedR)}],
	  Active,
	  [{style,Style}]}
	],
    {hframe,[{vframe,Dialog},
	     {vframe,[{button,done,[ok]},
		      {button,cancel,[cancel]}]}]}.

overlay_result(St) ->
    fun ([Style,Active,MinimizedL|Res]) -> 
	    ?DISPLAY(Res),
	    MinimizedC = proplists:get_value(minimized_c, Res),
	    MinimizedR = proplists:get_value(minimized_r, Res),
	    case lists:last(Res) of
		false ->
		    Dialog = mk_overlay_dialog(Style, Active, MinimizedL,
					       MinimizedC, MinimizedR), 
		    {dialog,Dialog,overlay_result(St)};
		true -> ignore
	    end
    end.

large_dialog_l(MinimizedL, MinimizedC) ->
    PaneColor = wings_pref:get_value(dialog_color),
    {vframe,
     [{label,"Label"},
      {key_alt,{d,1},"Alt 3",3,[{hook,disable_hook(c)}]},
      separator,
      {"Checkbox",false,[layout]},
      {"Checkbox key",false,[{key,c},{hook,disable_hook(-1)}]},
      separator,
      {key_alt,{d,1},"Alt 1",1,[{hook,disable_hook(c)}]},
      {custom,40,10,fun (X, Y, W, H, Store) ->
			    Color = case gb_trees:get(c, Store) of
					true -> {1,1,0};
					false -> {0,1,1} end,
			    wings_io:blend(PaneColor,
					   fun(Col) ->
						   wings_io:sunken_rect(
						     X, Y, W, H, Color, Col)
					   end)
		    end,[{hook,minimize_hook(-4)}]},
      {slider,[{range,{1,3}},{key,d},{hook,disable_hook(c)}]},
      {key_alt,{d,1},"Alt 2",2,[{hook,disable_hook(c)}]},
      separator,
      {custom,40,10,fun (X, Y, W, H, Store) ->
			    R = gb_trees:get(red, Store),
			    G = gb_trees:get(green, Store),
			    B = gb_trees:get(blue, Store),
			    wings_io:blend(PaneColor,
					   fun(Col) ->
						   wings_io:sunken_rect(
						     X, Y, W, H, {R,G,B}, 
						     Col)
					   end)
		    end},
      {hframe,
       [{vframe,[{label,"R"},{label,"G"},{label,"B"},
		 {label,"H"},{label,"S"},{label,"V"}]},
	{vframe,
	 [{slider,[{color,{r,green,blue}},{key,red},
		   {value,0.5},{range,{0.0,1.0}},
		   {hook,
		    color_update(r, {green,blue}, {hue,sat,val})}]},
	  {slider,[{color,{g,red,blue}},{key,green},
		   {value,0.5},{range,{0.0,1.0}},
		   {hook,
		    color_update(g, {red,blue}, {hue,sat,val})}]},
	  {slider,[{color,{b,red,green}},{key,blue},
		   {value,0.5},{range,{0.0,1.0}},
		   {hook,
		    color_update(b, {red,green}, {hue,sat,val})}]},
	  {slider,[{color,{h,sat,val}},{key,hue},
		   {value,300},{range,{0,360}},
		   {hook,
		    color_update(h, {sat,val}, {red,green,blue})}]},
	  {slider,[{color,{s,hue,val}},{key,sat},
		   {value,0.0},{range,{0.0,1.0}},
		   {hook,
		    color_update(s, {hue,val}, {red,green,blue})}]},
	  {slider,[{color,{v,hue,sat}},{key,val},
		   {value,0.5},{range,{0.0,1.0}},
		   {hook,
		    color_update(v, {hue,sat}, {red,green,blue})}]}
	 ]}],[{title,"Checkboxed Hframe"},checkbox,invert,
	      {minimized,MinimizedC},{key,minimized_c},
	      {hook,disable_hook(c)}]}],
     [{title,"Left Vframe"},{minimized,MinimizedL}]}.

large_dialog_r(MinimizedR) ->
    {vframe,
     [{text,123,[{hook,disable_hook(c)}]},
      {slider,{text,0.5,[{range,{0.0,1.0}}]}},
      {color,{1.0,0.0,0.0},[{hook,disable_hook(c)}]},
      {color,{0.0,1.0,0.0,1.0}},
      {menu,[{"Alt 1",1},{"Alt 2",2},{"Alt 3",3}],3,
       [{key,d},{hook,disable_hook(c)},{info,info(c)}]},
      {hframe,[{color,{0.0,0.0,1.0}},
	       panel,
	       {"Hide next frame",true,
		[{key,1},
		 {hook,fun (update, {Var,_I,Val,Store}) ->
			       {layout,gb_trees:update(Var, Val, Store)};
			   (_, _) -> void end}]}]},
      {hframe,[{text,1.23},
	       panel,
	       {button,"Ok",ok,[{hook,disable_hook(c)}]}],
       [{minimized,true}]},
      {menu,[{"A",a},{"B",b},{"C",c}],a,
       [{key,m},
	{hook,fun (menu_disabled, {_Var,_I,Sto}) ->
		      case gb_trees:get(c, Sto) of
			  true -> [];
			  _ -> [{b,[{info,info(c)}]}]
		      end;
		  (_, _) -> void
	      end},
       {info,"Partly disabled menu"}]},
      {button,"Reset",done,[{key,reset}]}
     ],[{title,"Right vframe"},{minimized,MinimizedR},
	{key,minimized_r},{hook,disable_hook(c)}]}.

info(c) -> "Requires \"Checkbox key\" checked".
    

color_update(T, {K1,K2}, {Ka,Kb,Kc}) ->
    fun (update, {Var,_I,Val,Store0}) ->
	    V1 = gb_trees:get(K1, Store0),
	    V2 = gb_trees:get(K2, Store0),
	    {Va,Vb,Vc} = color_update(T, Val, V1, V2),
	    Store1 = gb_trees:update(Var, Val, Store0),
	    Store2 = gb_trees:update(Ka, Va, Store1),
	    Store3 = gb_trees:update(Kb, Vb, Store2),
	    Store = gb_trees:update(Kc, Vc, Store3),
	    {store,Store};
	(_, _) ->
	    void
    end.

color_update(r, R, G, B) ->
    wings_ask:rgb_to_hsv(R, G, B);
color_update(g, G, R, B) ->
    wings_ask:rgb_to_hsv(R, G, B);
color_update(b, B, R, G) ->
    wings_ask:rgb_to_hsv(R, G, B);
color_update(h, H, S, V) ->
    wings_ask:hsv_to_rgb(H, S, V);
color_update(s, S, H, V) ->
    wings_ask:hsv_to_rgb(H, S, V);
color_update(v, V, H, S) ->
    wings_ask:hsv_to_rgb(H, S, V).

disable_hook(V) ->
    fun (is_disabled, {_Var,I,Store}) when integer(V) ->
	    not gb_trees:get(I+V, Store);
	(is_disabled, {_Var,_I,Store}) ->
	    R = not gb_trees:get(V, Store),
%%%	    io:format(?MODULE_STRING":~w ~p ~p~n", [?LINE,V,R]),
	    R;
	(_, _) ->
	    void
    end.

minimize_hook(V) ->
    fun (is_minimized, {_Var,I,Store}) when integer(V) ->
	    not gb_trees:get(I+V, Store);
	(is_minimized, {_Var,_I,Store}) ->
	    not gb_trees:get(V, Store);
	(_, _) ->
	    void
    end.



dynamic_dialog(St) -> 
    {dialog,Qs,Fun} = mk_dynamic_dialog(St, [init]),
    wings_ask:dialog("Dynamic Dialog",Qs,Fun).

check_dynamic_dialog(St, Res) -> 
    check_dynamic_dialog_1(St, Res, []).

check_dynamic_dialog_1(_St, [false], R0) -> 
    %% Dialog closed ok
    R = reverse(R0, [false]),
    ?DISPLAY(R),
    ignore;
check_dynamic_dialog_1(St, [true], R) -> %% New frame
    Z = true, E = false, F = 0.5, D = false,
    mk_dynamic_dialog(St, [false,D,F,E,Z|R]);
check_dynamic_dialog_1(St, [_Z,_E,_F,true|T], R) ->  % Delete frame
    mk_dynamic_dialog(St, reverse(T, R));
check_dynamic_dialog_1(St, [Z,E,F,false|T], R) -> % Keep frame
    check_dynamic_dialog_1(St, T, [false,F,E,Z|R]).

mk_dynamic_dialog(St, [_New|R]) ->
    mk_dynamic_dialog_1(St, R, [{hframe,[{button,"New",done},panel]}]).

mk_dynamic_dialog_1(St, [], Qs) ->
    {dialog,Qs,fun (R) -> check_dynamic_dialog(St, R) end};
mk_dynamic_dialog_1(St, [_D,F,E,Z|T], Qs0) ->
    Qs = 
	[{hframe,[{vframe,[{"Enable",E},
			   {text,F,[{range,{0.0,1.0}},
				    {hook,disable_hook(-1)}]}]},
		  {vframe,[{hframe,[{button,"Delete",done},panel]},
			  {slider,[{range,{0.0,1.0}},
				   {key,-5},
				   {hook,disable_hook(-6)}]}]}],
	  [{title,"Foo"},{minimized,Z}]}
	 |Qs0],
    mk_dynamic_dialog_1(St, T, Qs).

disabled_frames(_St) ->
    Qs =
	[{"Enable 1",false,[{key,enable1}]},
	 {"Enable 2",false,[{key,enable2}]},
	 {"Enable 3",false,[{key,enable3}]},
	 {vframe,
	  [{vframe,
	    [{vframe,
	      [{text,"Text field"},
	       {"Checkbox",false}],
	      [{title,"Frame 3"},%{minimized,false},
	       {hook,disable_hook(enable3)}]}],
	    [{title,"Frame 2"},{minimized,true},checkbox,invert,
	     {hook,disable_hook(enable2)}]},
	   {oframe,
	    [{"Text",{text,"Text Field"}},
	     {"Checkbox",{"Checkbox",false}}],
	    1,
	    [{hook,disable_hook(enable2)}]}],
	  [{title,"Frame 1"},%{minimized,false},
	   {hook,disable_hook(enable1)}]}],
%%%     Qs =
%%% 	[{"Enable",false,[{key,enable}]},
%%% 	 {vframe,
%%% 	  [{text,"Text field"},
%%% 	   {"Checkbox",false}],
%%% 	  [{title,"Disabled Frame"},{minimized,true},checkbox,invert,
%%% 	   {hook,disable_hook(enable)}]}],
    Fun =
	fun(Res) ->
		io:format("~p\n", [Res]),
		ignore
	end,
    wings_ask:dialog("Disabled Frames Dialog", Qs, Fun).
