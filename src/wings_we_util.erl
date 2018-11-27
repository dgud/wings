%%
%%  wings_we_util.erl --
%%
%%     Utilities for winged-edge records.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
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
    case catch array:get(Edge, Etab) of
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
    case catch array:get(Edge, Etab) of
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
    case {wings_util:array_keys(Vct),wings_util:array_keys(Vtab)} of
	{Same,Same} -> ok;
	{_,_} ->
	    crash(vc_and_vp_have_different_keys, We)
    end,
    array:sparse_foldl(fun(V, Edge, _) ->
			       case array:get(Edge, Etab) of
				   #edge{vs=V}=Rec ->
				       validate_edge_rec(Rec, We);
				   #edge{ve=V}=Rec ->
				       validate_edge_rec(Rec, We);
				   _Other ->
				       crash({vertex,V,Edge}, We)
			       end
		       end, [], Vct).

validate_edge_tab(#we{es=Etab}=We) ->
    array:sparse_foldl(fun(E, #edge{vs=Va,ve=Vb}, _) ->
			       verify_vertex(Va, E, We),
			       verify_vertex(Vb, E, We)
		       end, [], Etab).

validate_edge_rec(Rec, We) ->
    #edge{ltpr=LP,ltsu=LS,rtpr=RP,rtsu=RS} = Rec,
    if
	is_integer(LP+LS+RP+RS) -> ok;
	true -> crash({non_integer_edges,Rec}, We)
    end.

verify_vertex(V, Edge, #we{vc=Vct}=We) ->
    case array:get(V, Vct) of
	undefined ->
	    crash({edge,Edge,referenced,undefined,vertex,V}, We);
	E when is_integer(E) -> ok
    end.

crash(Reason, We) ->
    error({crash,get(where),Reason}, [We]).
