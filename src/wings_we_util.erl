%%
%%  wings_we_util.erl --
%%
%%     Utilities for winged-edge records.
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_we_util.erl,v 1.1 2004/12/16 20:05:16 bjorng Exp $
%%
-module(wings_we_util).
-export([validate/1]).

-include("wings.hrl").
-import(lists, [reverse/1,foreach/2]).

%%
%% Strong validation of the winged-edge structure.
%%

validate(#we{}=We) ->
    validate_edge_tab(We),
    validate_vertex_tab(We),
    validate_faces(We).
    
validate_faces(#we{fs=Ftab}=We) ->
    foreach(fun({Face,Edge}) ->
		    Cw = walk_face_cw(Face, Edge, Edge, We, []),
		    Ccw = walk_face_ccw(Face, Edge, Edge, We, []),
 		    case reverse(Ccw) of
 			Cw -> ok;
 			Other ->
 			    crash({{face,Face},
				   {cw,Cw},{ccw_reversed,Other}},
				  We)
 		    end,
		    case {lists:sort(Cw),ordsets:from_list(Cw)} of
			{Same,Same} -> ok;
			_ ->
			    crash({{face,Face},{repeated_vertex,Cw}}, We)
		    end
	    end,
	    gb_trees:to_list(Ftab)).

walk_face_cw(_Face, LastEdge, LastEdge, _We, [_|_]=Acc) -> Acc;
walk_face_cw(Face, Edge, LastEdge, We, Acc) ->
    #we{es=Etab} = We,
    case catch gb_trees:get(Edge, Etab) of
	#edge{vs=V,lf=Face,ltsu=Next} ->
	    walk_face_cw(Face, Next, LastEdge, We, [V|Acc]);
	#edge{ve=V,rf=Face,rtsu=Next} ->
	    walk_face_cw(Face, Next, LastEdge, We, [V|Acc]);
	{'EXIT',_} ->
	    [{make_ref(),crash,missing_edge,Edge,
	     [Face,Edge,LastEdge,We,Acc]}];
	Other ->
	    [{make_ref(),{crash,Other},
	      {face,Face,edge,Edge,last_edge,LastEdge,acc,Acc}}]
    end.

walk_face_ccw(_Face, LastEdge, LastEdge, _We, [_|_]=Acc) -> Acc;
walk_face_ccw(Face, Edge, LastEdge, We, Acc) ->
    #we{es=Etab} = We,
    case catch gb_trees:get(Edge, Etab) of
	#edge{ve=V,lf=Face,ltpr=Next} ->
	    walk_face_ccw(Face, Next, LastEdge, We, [V|Acc]);
	#edge{vs=V,rf=Face,rtpr=Next} ->
	    walk_face_ccw(Face, Next, LastEdge, We, [V|Acc]);
	{'EXIT',_} ->
	    [{make_ref(),crash,missing_edge,Edge,
	     [Face,Edge,LastEdge,We,Acc]}];
	Other ->
	    [{make_ref(),{crash,Other},
	      {face,Face,edge,Edge,last_edge,LastEdge,acc,Acc}}]
    end.

validate_vertex_tab(#we{es=Etab,vc=Vct,vp=Vtab}=We) ->
    case {gb_trees:keys(Vct),gb_trees:keys(Vtab)} of
	{Same,Same} -> ok;
	{_,_} ->
	    crash(vc_and_vp_have_different_keys, We)
    end,
    foreach(fun({V,Edge}) ->
		    case gb_trees:get(Edge, Etab) of
			#edge{vs=V}=Rec ->
			    validate_edge_rec(Rec, We);
			#edge{ve=V}=Rec ->
			    validate_edge_rec(Rec, We);
			_Other ->
			    crash({vertex,V}, We)
		    end
	    end,
	    gb_trees:to_list(Vct)).

validate_edge_tab(#we{es=Etab}=We) ->
    foreach(fun({E,#edge{vs=Va,ve=Vb}}) ->
		    verify_vertex(Va, E, We),
		    verify_vertex(Vb, E, We)
	    end,
	    gb_trees:to_list(Etab)).

validate_edge_rec(Rec, We) ->
    #edge{ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} = Rec,
    if
	integer(LP+LS+RP+RS) -> ok;
	true -> crash({non_integer_edges,Rec}, We)
    end.

verify_vertex(V, Edge, #we{vc=Vct}=We) ->
    case gb_trees:is_defined(V, Vct) of
	false ->
	    crash({edge,Edge,referenced,undefined,vertex,V}, We);
	true -> ok
    end.

crash(Reason, We) ->
    erlang:error({crash,get(where),Reason}, [We]).
