%%
%%  wings_ask.hrl --
%%
%%     Global record definition and defines for dialog controls.
%%
%%  Copyright (c) 2013 Micheus
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

%% Static data for every field.
%% *** copied from src/wings_ask.erl - it must be the same ***
-record(fi,
    {handler,			%Handler fun.
     key=0,				%Field key.
     index,				%Field index
     state=inert,		%inert|disabled|enabled
     minimized,			%true|false|undefined
     hook,				%Field hook fun/2
     flags=[],			%Flags field.
     x,y,				%Upper left position.
     w,h,				%Width, height.
     stretch=0,			%Horizontal stretch koefficient
     extra				%Container or leaf data
    }).


