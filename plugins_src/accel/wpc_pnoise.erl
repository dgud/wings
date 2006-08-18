%%
%%  wpc_pnoise.erl --
%%
%%     Plug-in for accelerating perlin noise (just loads the driver)
%%
%%  Copyright (c) 2005 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_pnoise.erl,v 1.1 2006/01/14 09:02:38 dgud Exp $
%%


-module(wpc_pnoise).

-export([init/0]).

init() ->
    pnoise:start(),
    false.

