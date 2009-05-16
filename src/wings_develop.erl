%%
%%  wings_develop.erl --
%%
%%     This module implements a Develop menu with useful functions
%%     for Wings and plug-in developers.
%%
%%  Copyright (c) 2009 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_develop).
-export([menu/1,command/2]).

-include("wings.hrl").

menu(_) ->
    [{"(Nothing here yet)",ignore}].

command(ignore, St) ->
    St.
