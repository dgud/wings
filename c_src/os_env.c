/*
 *  os_env.c --
 *
 *     Set environment function
 *
 *  Copyright (c) 2022 Dan Gudmundsson
 *
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#ifdef __WIN32__
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif
#include <string.h>

#include "erl_nif.h"

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_badarg;
static ERL_NIF_TERM atom_error;

static ERL_NIF_TERM os_set_env(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM os_get_env(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
  {
   {"set_nif", 2, os_set_env},
   {"get_nif", 1, os_get_env},
  };

#define make_error(TYPE, Desc)                                          \
    {                                                                   \
        return enif_raise_exception(env,                                \
                                    enif_make_tuple2(env, TYPE, enif_make_string(env, Desc, ERL_NIF_LATIN1))); \
    }

static ERL_NIF_TERM os_set_env(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary var, value;
  if(!enif_inspect_binary(env, argv[0], &var)) make_error(atom_badarg, "Var");
  if(!enif_inspect_binary(env, argv[1], &value)) make_error(atom_badarg, "Value");

#ifdef __WIN32__
  if(!SetEnvironmentVariableW((LPCWSTR)var.data, (LPCWSTR) value.data))
    make_error(atom_error, "Could not set variable");
#else
  if(setenv(var.data, value.data, 1)) make_error(atom_error, "Could not set variable");
#endif
  return atom_ok;
}

static ERL_NIF_TERM os_get_env(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary var;
  ERL_NIF_TERM bin;
  char value[16384], *dest, *src;
  size_t len;
  if(!enif_inspect_binary(env, argv[0], &var)) make_error(atom_badarg, "Var");

#ifdef __WIN32__
  if(!GetEnvironmentVariableW((LPCWSTR)var.data, (LPWSTR) value, 16384 / 2)) {
    if(ERROR_ENVVAR_NOT_FOUND == GetLastError()) {
      value[0] = 0; value[1] = 0;
    } else make_error(atom_error, "Could not get variable");
  }
  len = wcslen((wchar_t*) value)*2;
  src = value;
#else
  src = getenv(var.data);
  if(!src) {
    value[0] = 0;
    src = value;
  }
  len = strlen(src);
#endif

  dest = enif_make_new_binary(env, len, &bin);
  memcpy(dest, src, len);

  return bin;
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  atom_ok = enif_make_atom(env, "ok");
  atom_badarg = enif_make_atom(env, "badarg");
  atom_error = enif_make_atom(env, "error");
  return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{

}

ERL_NIF_INIT(os_env,nif_funcs,load,NULL,upgrade,unload)
