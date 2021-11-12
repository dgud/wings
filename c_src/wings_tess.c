/*
 *  wings_tess.c --
 *
 *     Erlang nif for glu tesselation
 *
 *  Copyright (c) 2021 Dan Gudmundsson & Björn Gustavsson
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
#include "tess/wtess.h"

static ERL_NIF_TERM triangulate(ErlNifEnv *, int argc, const ERL_NIF_TERM *);

ERL_NIF_TERM ATOM_BADARG;
ERL_NIF_TERM ATOM_NORMAL;
ERL_NIF_TERM ATOM_VS;

#define BADARG(ARG) \
  {return enif_raise_exception(env, enif_make_tuple2(env, ATOM_BADARG, ARG));}


static ErlNifFunc nif_funcs[] =
  {
   {"triangulate", 2, triangulate, 0},
  };


typedef struct {
    GLdouble * tess_coords;
    int alloc_n;
    int alloc_max;

    int * tess_index_list;
    int index_n;
    int index_max;

    int error;
} egl_tess_data;

#define NEED_MORE_ALLOC 1
#define NEED_MORE_INDEX 2

void egl_ogla_vertex(GLdouble* coords, egl_tess_data *egl_tess)
{
  /* fprintf(stderr, "%d\r\n", (int) (coords - tess_coords) / 3); */
  if(egl_tess->index_n < egl_tess->index_max) {
    egl_tess->tess_index_list[egl_tess->index_n] = (int) (coords - egl_tess->tess_coords) / 3;
    egl_tess->index_n++;
  }
  else
    egl_tess->error = NEED_MORE_INDEX;
}

void egl_ogla_combine(GLdouble coords[3],
                 void* vertex_data[4],
                 GLfloat w[4],
                 void **dataOut,
                 egl_tess_data *egl_tess)
{
  GLdouble* vertex = &egl_tess->tess_coords[egl_tess->alloc_n];
  if(egl_tess->alloc_n < egl_tess->alloc_max) {
    egl_tess->alloc_n += 3;
    vertex[0] = coords[0];
    vertex[1] = coords[1];
    vertex[2] = coords[2];
    *dataOut = vertex;

  } else {
    egl_tess->error = NEED_MORE_ALLOC;
    *dataOut = NULL;
  }
}

void egl_ogla_edge_flag(GLboolean flag, void *user_data)
{
}

void  egl_ogla_error(GLenum errorCode, void *user_data)
{
  // const GLubyte *err;
  // err = gluErrorString(errorCode);
  // fprintf(stderr, "Tesselation error: %d: %s\r\n", (int) errorCode, err);
}

static ERL_NIF_TERM triangulate(ErlNifEnv* env, int argc , const ERL_NIF_TERM argv[])
{
  int i, a;
  unsigned int num_vertices;
  GLdouble n[3], *vs;
  ErlNifBinary bin;
  const ERL_NIF_TERM *tuple;
  ERL_NIF_TERM vs_l, vs_h, vs_t, reply;
  GLUtesselator* tess;
  egl_tess_data egl_tess;

  int a_max = 2;
  int i_max = 6;

  if(!enif_get_tuple(env, argv[0], &a, &tuple) && a != 3) BADARG(ATOM_NORMAL);
  if(!enif_get_double(env, tuple[0], &n[0])) BADARG(ATOM_NORMAL);
  if(!enif_get_double(env, tuple[1], &n[1])) BADARG(ATOM_NORMAL);
  if(!enif_get_double(env, tuple[2], &n[2])) BADARG(ATOM_NORMAL);

  if(!enif_get_list_length(env, argv[1], &num_vertices)) BADARG(ATOM_VS);

  egl_tess.alloc_max = a_max*num_vertices*3;
  egl_tess.error = 0;
  enif_alloc_binary(egl_tess.alloc_max*sizeof(GLdouble), &bin);
  vs = (GLdouble *) bin.data;
  egl_tess.tess_coords = vs;

  vs_l = argv[1];
  while(enif_get_list_cell(env,  vs_l, &vs_h, &vs_t)) {
    if(!enif_get_tuple(env, vs_h, &a, &tuple) && a != 3) BADARG(ATOM_VS);
    if(!enif_get_double(env, tuple[0], vs++)) BADARG(ATOM_VS);
    if(!enif_get_double(env, tuple[1], vs++)) BADARG(ATOM_VS);
    if(!enif_get_double(env, tuple[2], vs++)) BADARG(ATOM_VS);
    vs_l = vs_t;
  }
  egl_tess.index_max = i_max*3*num_vertices;
  egl_tess.tess_index_list = (int *) enif_alloc(sizeof(int) * egl_tess.index_max);

  egl_tess.index_n = 0;
  egl_tess.alloc_n = num_vertices*3;

  tess = gluNewTess();

  gluTessCallback(tess, GLU_TESS_VERTEX_DATA,     (_GLUfuncptr) egl_ogla_vertex);
  gluTessCallback(tess, GLU_TESS_EDGE_FLAG_DATA,  (_GLUfuncptr) egl_ogla_edge_flag);
  gluTessCallback(tess, GLU_TESS_COMBINE_DATA,    (_GLUfuncptr) egl_ogla_combine);
  gluTessCallback(tess, GLU_TESS_ERROR_DATA,      (_GLUfuncptr) egl_ogla_error);

  gluTessNormal(tess, n[0], n[1], n[2]);
  gluTessBeginPolygon(tess, &egl_tess);
  gluTessBeginContour(tess);
  for (i = 0; i < num_vertices; i++) {
    gluTessVertex(tess, egl_tess.tess_coords+3*i, egl_tess.tess_coords+3*i);
  }
  gluTessEndContour(tess);
  gluTessEndPolygon(tess);

  vs_t = enif_make_list(env, 0);
  i=egl_tess.index_n;
  while(i > 0) {
    i--;
    vs_t = enif_make_list_cell(env, enif_make_int(env, egl_tess.tess_index_list[i]), vs_t);
  };

  enif_realloc_binary(&bin, egl_tess.alloc_n*sizeof(GLdouble));
  reply = enif_make_tuple2(env, vs_t, enif_make_binary(env, &bin));
  /* fprintf(stderr, "List %d: %d %d %d \r\n",  */
  /* 	  res, */
  /* 	  n_pos,  */
  /* 	  (tess_alloc_vertex-new_vertices)*sizeof(GLdouble),  */
  /* 	  num_vertices*6*sizeof(GLdouble)); */
  enif_free(egl_tess.tess_index_list);
  gluDeleteTess(tess);
  return reply;
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  ATOM_BADARG      = enif_make_atom(env,"badarg");
  ATOM_NORMAL  = enif_make_atom(env,"normal");
  ATOM_VS      = enif_make_atom(env,"vs");

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

ERL_NIF_INIT(wings_glu_tess,nif_funcs,load,NULL,upgrade,unload)
