%%
%%  wp7_color_ramp.hrl --
%%
%%     Global record definition and defines.
%%
%%  Copyright (c) 2013 Micheus
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-define(CR_MAX, 200).
-define(CR_START, 3).
-define(CR_W, ?CR_MAX-1).  % 0-100 => x2 => (100x2)-1 = 199
-define(CR_H, 12).
-define(CR_WIDTH, ?CR_W+2*?CR_START).
-define(CR_HEIGHT, ?CR_H+2*?CR_START).

-type color() :: {float(),float(),float()}.

%% Key information data structure
-record(ki,
    {idx=0 :: integer(),
     col={0.0,0.0,0.0} :: color()
    }).

%% Color ramp data structure
-record(cr,
    {name=custom :: atom(),
     color_keys :: erlang:orddict()
    }).

