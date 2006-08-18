%%%-------------------------------------------------------------------
%%% File    : auv.hrl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : Records for texturing
%%%
%%% Created :  3 Oct 2002 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
%%  Copyright (c) 2001-2004 Dan Gudmundsson 
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%     $Id: auv.hrl,v 1.39 2006/01/26 23:17:31 dgud Exp $

%% Chart record (one for each chart).
%% Stored in the 'name' field in the #we{} record.
-record(ch,
	{size,
	 vmap,			 %Map back to original vertex numbers.
	 emap,			 %Map back to original edge numbers.
	 me			 %Missing edges.
	}).

-record(uvstate,
	{matname,          %% The textured MatName
	 bg_img,           %% The background image
	 id,               %% The we id of the shape we are working with.
	 mode=object,      %% object mode or a gb_sets of faces which we are editing
	 st,               %% Wings working 'st', i.e. no autouv stuff in this one
	 orig_st           %% A copy of entering 'st', to keep orig
	                   %% uv-coords/vertex-colors and materials. 
	}).

-define(HOLE, 'Ignore Chart').

-ifdef(DEBUG).
-define(DBG(S,A), io:format("~p:~p " ++ S, [?MODULE,?LINE|A])).
-else.
-define(DBG(S,A), ok).
-endif.

%% auv_placement:group_edge_loops(Faces, We) delivers a list of these
-record(be,{vs,ve,edge,face,cut,dist}).
