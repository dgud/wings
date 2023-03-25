%%
%%  wings_pick_nif.erl --
%%
%%     This module does glu tesselation.
%%
%%  Copyright (c) 2021 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Remove the need of glu (included in wx) also enables us tesselate in parallel
%%

-module(wings_glu_tess).
-export([triangulate/2]).

-on_load(init/0).

-define(nif_stub,nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

init() ->
    case get(wings_not_running) of
	undefined ->
            Name = "wings_tess",
	    Dir = case code:priv_dir(wings) of
                      {error, _} -> filename:join(wings_util:lib_dir(wings),"priv");
                      Priv -> Priv
                  end,
            Nif = filename:join(Dir, Name),
            erlang:load_nif(Nif, 0);
	_ ->
	    false
    end.

%% @doc General purpose polygon triangulation.
%% The first argument is the normal and the second a list of
%% vertex positions. Returned is a list of indices of the vertices
%% and a binary (64bit native float) containing an array of
%% vertex positions, it starts with the vertices in Vs and
%% may contain newly created vertices in the end.
-spec triangulate(Normal, [Vs]) -> {[TriIndex], [VPos]}
              when Normal :: e3d_vec:vector(), Vs :: e3d_vec:point(),
                   TriIndex :: {integer(),integer(),integer()}, VPos ::e3d_vec:point().
triangulate(_Normal, _Vs) ->
    ?nif_stub.
