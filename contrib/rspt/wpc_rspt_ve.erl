%%
%%  Copyright (c) 2004 Rolf Stenholm
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_rspt_ve.erl,v 1.1 2004/08/25 05:33:06 bjorng Exp $
%%

%
%
%vector module
%
%
%
-module(wpc_rspt_ve).
-define(NEED_ESDL,1).
-define(NEED_OPENGL,1).
-include_lib("wings.hrl").
-include_lib("e3d.hrl").
-include_lib("e3d_image.hrl").
-export([init/0,menu/2,command/2]).
-export([normal/3,normal2/2,normalize/1,cross_product/2,add/2,square_length/1,create/3,dot_product/2,scalar_product/2]).

-define(debugP(X,W),ok).
-define(debugR(X,W),ok).
%-define(debugR(X,W),io:format(X,W)).
-define(debugM(X,W),ok).
%-define(debugM(X,W),io:format(X,W)).
-define(debug2(X,W),ok).
%-define(debugN(X,W),io:format(X,W)).
-define(debugN(X,W),ok).

%% link into app
init()->
	%%io:format("Loading wpc_rspt_ve ... ~n",[]),
	true.

menu(_, Menu)->
	Menu.

command(_,_)-> next.

%%%%%%%%%%%
%%%%%%%%%%% Vector rubbish from wpc_reduce
%%%%%%%%%%% Somehat specialized rubbish

normal(VX,Face,#we{fs=FS}=WE)->

	normal2(VX,WE).


% called vector normal should be vertex normal
normal2(VX,#we{vp=VP}=WE)->
	%% grab all adjacent edges
	Edges = wpc_rspt_sc:grab_adjacent_edges(VX,WE),
	%% fold compute normals over edges
	Normal = lists:foldl(fun({X,_},CVal)->
				Nnormal = wpc_rspt_sc:surface_normal(X,WE),
				Normalized = normalize(Nnormal),
				add(CVal,Normalized)
			end,{0,0,0},Edges),
	%% normalize vectors (slow..)
	
	scalar_product(normalize(Normal),-1).
	% debug rubbish (works for cube and sphere as Normal ... )
	%normalize(gb_trees:get(VX,VP)).

normalize({X1,X2,X3}=X)->
	L  = square_length(X),
	NF = math:sqrt(L),
	Vec = case NF<0.0000000001 of
		true ->
			?debugN("============= Slashing normal ==========~n",[]),
			{0,0,0};
		false->
			{X1/NF,X2/NF,X3/NF}
	      end,
	?debugN("Normal ~p~n",[Vec]),
	Vec.

cross_product({X1,X2,X3},{Y1,Y2,Y3})->
	{X2*Y3-X3*Y2,X3*Y1-X1*Y3,X1*Y2-X2*Y1}.

add({X1,X2,X3},{Y1,Y2,Y3})->
	{X1+Y1,X2+Y2,X3+Y3}.

square_length(X)->
	dot_product(X,X).

create(VS,VE,#we{vp=VP}=WE)->
	{X1,X2,X3}  = gb_trees:get(VE,VP),
	{Y1,Y2,Y3}  = gb_trees:get(VS,VP),
	{X1-Y1,X2-Y2,X3-Y3}.

dot_product({X1,X2,X3}=X,{Y1,Y2,Y3}=Y)->
	X1*Y1+X2*Y2+X3*Y3.

scalar_product({X1,X2,X3},S)->
	{X1*S,X2*S,X3*S}.
