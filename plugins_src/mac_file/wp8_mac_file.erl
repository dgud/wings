%%
%%  wp8_file.erl --
%%
%%     Native file dialog boxes for Mac OS X.
%%
%%  Copyright (c) 2001-2013 Patrik Nyblom, Bjorn Gustavsson.
%%
%%  Changes for OSX by Sean Hinde : 2002/2/18
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wp8_mac_file).

-export([init/1]).

-define(NEED_ESDL, 1).
-include("wings.hrl").

%% Operations supported by driver.
-define(OP_READ, 1).
-define(OP_WRITE, 2).

init(Next) ->
    case os:type() of
	{unix,darwin} ->
	    fun(What) ->
		    fileop(What, Next)
	    end;
	_ ->
	    Next
    end.

fileop(What, Next) ->
    case wpa:pref_get(wpc_mac_misc, native_file_dialog) of
	true -> fileop_1(What, Next);
	false -> Next(What)
    end.

fileop_1({file,open_dialog,Prop,Cont}, _Next) ->
    Title = proplists:get_value(title, Prop, ?__(1,"Open")),
    Dir = proplists:get_value(directory, Prop),
    case file_dialog(open, Dir, Prop, Title) of
	aborted -> keep;
	Res -> Cont(Res)
    end;
fileop_1({file,save_dialog,Prop,Cont}, _Next) ->
    Title = proplists:get_value(title, Prop, ?__(2,"Save")),
    Dir = proplists:get_value(directory, Prop),
    case file_dialog(save, Dir, Prop, Title) of
	aborted -> keep;
	Res -> Cont(Res)
    end;
fileop_1(What, Next) ->
    Next(What).

file_dialog(Type, Dir, Prop, Title) ->
    wait_for_modifiers_up(),
    DefName = proplists:get_value(default_filename, Prop, ""),
    Filters = file_filters(Prop),
    
    %% Disabling the key repeat here and then enable it again
    %% seems to get rid of the annoying problem with repeating
    %% dialog boxes.
    sdl_keyboard:enableKeyRepeat(0, 0),
    DlgProps = [{operation,Type},{title,Title},{directory,Dir},
		{default_filename,DefName},{filters,Filters}],
    Res = case sdl_video:wm_mac_file_dialog(DlgProps) of
	      [] -> aborted;
	      Other -> filename:absname(Other)
	  end,
    sdl_keyboard:enableKeyRepeat(?SDL_DEFAULT_REPEAT_DELAY,
				 ?SDL_DEFAULT_REPEAT_INTERVAL),
    sdl_keyboard:setModState(0),
    Res.

file_filters(Prop) ->
    case proplists:get_value(extensions, Prop, none) of
	none ->
	    [proplists:get_value(ext, Prop, ".wings")];
	Exts ->
	    [Ext || {Ext,_Desc} <- Exts]
    end.

wait_for_modifiers_up() ->
    case sdl_keyboard:getModState() == 0 andalso no_key_pressed() of
	true -> ok;
	false ->
	    receive after 10 -> ok end,
	    sdl_events:peepEvents(),
	    wait_for_modifiers_up()
    end.

no_key_pressed() ->
    no_key_pressed(1, sdl_keyboard:getKeyState()).

no_key_pressed(I, T) when element(I, T) =/= 0 -> false;
no_key_pressed(I, T) when I =< size(T) -> no_key_pressed(I+1, T);
no_key_pressed(_, _) -> true.
