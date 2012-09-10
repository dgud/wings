%%
%%  wpax.hrl --
%%
%%     wpax.erl record definition and defines.
%%
%%  Copyright (c) 2012 Micheus L Vieira
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-include("e3d.hrl").

-define(DEGREE2RAD, 0.01745329251994323).
-define(RAD2DEGREE, 57.2957795130823209).

-type vtx_location() :: 'none' | e3d_vector().
-record(vl,
    {v=none :: vtx_location(),    % vertex location 
     vn=none :: vtx_location()}). % vertex normal

-type vl() :: 'none' | #vl{}.
-record(seg_inf,        % segment information
    {va=none :: vl(),   % begin vertex
     vb=none :: vl(),   % end vertex
     vc=none :: vl()}). % current vertex

