%%
%%  wpc_isometric_view.erl --
%%
%%     Plug-in for adjust the camera to match with the isometric view
%%
%%  CopyRight (c) 2019 Micheus
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wpc_isometric_view).
%% Plugin entry points
-export([init/0,menu/2,command/2]).

-include_lib("wings/src/wings.hrl").

init() ->
    true.

menu({view}, Menu) ->
    PatchMenu = fun({_, orthogonal_view, _, _} = Entry) ->
		    [Entry, isometric_menu()];
		   (Entry) -> Entry
		end,
    lists:flatten([PatchMenu(Entry) || Entry <- Menu]);
menu(_, Menu) -> Menu.

isometric_menu() ->
    {?__(1,"Isometric View"),isometric_view,
     ?__(2,"Reset the view and set it to isometric viewing")}.


command({view,isometric_view}, St0) ->
    case wings_wm:is_geom() of
	true ->
	    View = wings_view:current(),
	    wings_view:set_current(View#view{azimuth=-45.0,elevation=35.264}),
	    case ortogonal_active() of
		true -> St0;
		false ->  wings:command({view,orthogonal_view},St0)
	    end;
	_ -> St0
    end;
command(_Cmd, _) -> next.


ortogonal_active() ->
    case wings_wm:lookup_prop(orthogonal_view) of
	none -> wings_pref:get_value(orthogonal_view, false);
	{value,Bool} -> Bool
    end.
