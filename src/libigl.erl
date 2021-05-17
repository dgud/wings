%%
%%  libigl.erl --
%%
%%     This module interfaces some functions in libigl.
%%
%%  Copyright (c) 2019 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.

-module(libigl).

-export([lscm/4]).
-export([harmonic/4]).
-export([slim/5]).
-on_load(init/0).
-define(nif_stub,nif_stub_error(?LINE)).

-type triangles() :: nonempty_list().
-type point2d() :: {float(), float()}.

-type energy_type() :: 'arap' | 'log_arap' | 'symmetric_dirichlet' |
                       'conformal' | 'exp_conformal' | 'exp_symmetric_dirichlet'.

%% Least square conformal maps
%% At least bind two BorderVs with UV-positions in BorderPos
-spec lscm(Vs::[e3d_vec:point()],Fs::triangles(),[BVs::integer()], BPos::[point2d()]) ->
          [point2d()] | {'error', term()} | 'false'.
lscm(_Vs,_Fs,_BorderVs,_BorderPos) ->
    ?nif_stub.


%% Harmonic mapping
%% All BorderVs must be present and have an initial mapping in BorderPos
-spec harmonic(Vs::[e3d_vec:point()],Fs::triangles(),[BVs::integer()], BPos::[point2d()]) ->
                  [point2d()].
harmonic(_Vs,_Fs,_BorderVs,_BorderPos) ->
    ?nif_stub.


%% Slim Scalable Locally Injective Mappings
%% UVInitPos must contain an initial (non-flipped) UV-mapping of all verts
%% Stops when change is less then eps
-spec slim(VS::[e3d_vec:point()], Fs::triangles(),
           UVInitPos::[point2d()], energy_type(), Eps::float()) -> [point2d()].
slim(_Vs,_Fs, _InitUVs, _Type, _Eps) ->
    ?nif_stub.

%%% Nif init

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

init() ->
    case get(wings_not_running) of
	undefined ->
            Name = "libigl",
	    Dir = case code:priv_dir(wings) of
                      {error, _} -> filename:join(wings_util:lib_dir(wings),"priv");
                      Priv -> Priv
                  end,
            Nif = filename:join(Dir, Name),
            erlang:load_nif(Nif, 0);
	_ ->
	    false
    end.

