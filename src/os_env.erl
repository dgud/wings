%%
%%  os_env.c --
%%
%%     Set OS environment function
%%
%%       Sets the process env variables,
%%       erlangs os:setenv() only sets in its internal copy
%%       due to that setenv is not thread safe on linux.
%%       But sometimes we just need to take a chance, :-(
%%       for example to set LD_LIBRARY_PATH to load 'so' files or
%%       set PATH on windows to load 'dll's.
%%
%%  Copyright (c) 2022 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(os_env).

-export([set/2, get/1]).
-on_load(init/0).

-define(nif_stub,nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

init() ->
    Name = "os_env",
    Dir = case code:priv_dir(wings) of
              {error, _} -> filename:join(wings_util:lib_dir(wings),"priv");
              Priv -> Priv
          end,
    Nif = filename:join(Dir, Name),
    erlang:load_nif(Nif, 0).

-spec set(string(), string()) -> ok.
set(Var, Value) ->
    case  os:type() of
        {win32, _} ->
            set_nif(unicode:characters_to_binary([Var,0], unicode, {utf16, little}),
                    unicode:characters_to_binary([Value,0], unicode, {utf16, little}));
        _ ->
            set_nif(unicode:characters_to_binary([Var,0]),
                    unicode:characters_to_binary([Value, 0]))
    end.
set_nif(_Var, _Value) ->
    ?nif_stub.

-spec get(string()) -> string().
get(Var) ->
    case os:type() of
        {win32, _} ->
            Bin = get_nif(unicode:characters_to_binary([Var,0], unicode, {utf16, little})),
            unicode:characters_to_list(Bin, {utf16, little});
        _ ->
            Bin = get_nif(unicode:characters_to_binary([Var,0])),
            unicode:characters_to_list(Bin)
    end.
get_nif(_Var) -> ?nif_stub.
