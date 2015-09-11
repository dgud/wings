%%
%%  wpc_pnoise.erl --
%%
%%     Plug-in for accelerating perlin noise (just loads the driver)
%%
%%  Copyright (c) 2005-2011 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%


-module(wpc_pnoise).

-export([init/0]).

init() ->
    case get(wings_not_running) of
	undefined -> pnoise:start();
	_ -> ignore
    end,
    false.

