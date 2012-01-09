%%
%%  wp8_file.erl --
%%
%%     Native file dialog boxes for Win32.
%%
%%  Copyright (c) 2001 Patrik Nyblom
%%                2002-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wp8_file).

-export([menus/0, init/1]).

-include("wings_intl.hrl").

%% Operations supported by driver.
-define(OP_READ, 1).
-define(OP_WRITE, 2).
-define(OP_MAXIMIZE, 3).

menus() -> [].

init(Next) ->
    case os:type() of
	{win32,_} ->
	    Dir = filename:dirname(code:which(?MODULE)),
	    case erl_ddll:load_driver(Dir, "wings_file_drv") of
		ok ->
		    case catch open_port({spawn,"wings_file_drv"}, []) of
			Port when is_port(Port) ->
			    %% maybe_maximize_window(Port),
			    register(wp8_file_port, Port),
			    fun(What) ->
				    fileop(What,Next)
			    end;
			_ -> Next
		    end;
		_ -> Next
	    end;
	_ -> Next
    end.

fileop({file,open_dialog,Prop,Cont}, _Next) ->
    Title = proplists:get_value(title, Prop, ?__(1,"Open")),
    file_dialog(?OP_READ, Prop, Title, Cont);
fileop({file,save_dialog,Prop,Cont}, _Next) ->
    Title = proplists:get_value(title, Prop, ?__(2,"Save")),
    file_dialog(?OP_WRITE, Prop, Title, Cont);
fileop(What, Next) ->
    Next(What).

file_dialog(Type, Prop, Title, Cont) ->
    %% We create a dummy window here so that no file
    %% file dialogs on any platform will execute in the geom windows,
    %% to help the Windows programmers get their plug-ins
    %% working on other platforms.

    Op = {push,fun file_dialog_event/1},
    Name = dummy_file_dialog_window,
    wings_wm:new(Name, {0,0,1}, {0,0}, Op),
    wings_wm:hide(Name),
    wings_wm:send(Name, {do_dialog,Type,Prop,Title,Cont}).

file_dialog_event({do_dialog,Type,Prop,Title,Cont}) ->
    Dir = proplists:get_value(directory, Prop),
    case file_dialog_1(Type, Dir, Prop, Title) of
	aborted -> keep;
	Res -> Cont(Res)
    end,
    delete;
file_dialog_event(_) -> keep.

file_dialog_1(Type, Dir0, Prop, Title) ->
    DefName = proplists:get_value(default_filename, Prop, ""),
    {ok,Cwd} = file:get_cwd(),
    file:set_cwd(Dir0),
    Filters = file_filters(Prop),
    Dir = filename:nativename(Dir0),
    Data = [Dir,0,Title,0,DefName,0|Filters],
    case erlang:port_control(wp8_file_port, Type, Data) of
	[] ->
	    file:set_cwd(Cwd),
	    aborted;
	Else ->
	    file:set_cwd(Cwd),
	    filename:absname(Else) % Happens to turn windows slashes...
    end.

file_filters(Prop) ->
    Exts = case proplists:get_value(extensions, Prop, none) of
	       none ->
		   Ext = proplists:get_value(ext, Prop, ".wings"),
		   ExtDesc = proplists:get_value(ext_desc, Prop,
						 ?__(1,"Wings File")),
		   [{Ext,ExtDesc}];
	       Other -> Other
	   end,
    [file_add_all(Exts),file_filters_1(Exts++[{".*", ?__(2,"All Files")}], [])].

file_filters_1([{Ext,Desc}|T], Acc0) ->
    Wildcard = "*" ++ Ext,
    Acc = [Acc0,Desc," (",Wildcard,")",0,Wildcard,0],
    file_filters_1(T, Acc);
file_filters_1([], Acc) -> [Acc,0].
    
file_add_all([_]) -> [];
file_add_all(Exts) ->
    All0 = ["*"++E || {E,_} <- Exts],
    All = file_add_semicolons(All0),
    [?__(1,"All Formats")++" (",All,")",0,All,0].

file_add_semicolons([E1|[_|_]=T]) ->
    [E1,";"|file_add_semicolons(T)];
file_add_semicolons(Other) -> Other.

%% Just because we have driver... we have piggybacked the implementation
%% of Windows maximzation to it.
maybe_maximize_window(Port) ->
    case wings_pref:get_value(win32_start_maximized, false) of
	false ->
	    ok;
	true ->
	    %% The handle is in big-endian format (not documented).
	    %% We will convert it to little endian format to do
	    %% the Maxmize command maximally lazy in the driver.
	    {_,<<Handle0:32>>} = sdl_video:wm_getInfo(),
	    Handle = <<Handle0:32/little>>,
	    erlang:port_control(Port, ?OP_MAXIMIZE, Handle)
    end.
