%%
%%  wings_intl.hrl --
%%
%%     Defines for translations
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%
-compile({parse_transform,tools}).

-define(STR(A,B,Str), wings_lang:str({?MODULE,A,B},Str)).
-define(__(Key,Str), wings_lang:str({?MODULE,?FUNCTION_NAME,Key},Str)).
