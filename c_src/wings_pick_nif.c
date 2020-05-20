/*
 *  wings_pick_nif.c --
 *
 *     Erlang nif for picking.
 *
 *  Copyright (c) 2009-2019 Bjorn Gustavsson
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

struct vertex_struct {
  float x, y, w, z;
};
typedef struct vertex_struct vertex;
#define PRE_RES_SIZE 512
#define MAX_RES_SIZE 1024*512

/* Declarations of internal functions */
static ERL_NIF_TERM pick(float* vertices, unsigned stride, unsigned num_tris,
                         float* m, int, int, int,
                         ErlNifEnv* env);
static void mul(vertex* out, float x, float y, float z, float m[16]);
static void intersection(vertex* out, vertex* prev_vp, vertex* cur_vp,
			 float bc_prev, float bc_cur);
static int do_cull(vertex* vp, int ccw);
#if 0
static void print_tri(vertex* tri);
static void print_vertex(vertex* v);
#endif

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;

static ERL_NIF_TERM faces(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
  {
   {"faces_1", 6, faces},
  };


static ERL_NIF_TERM faces(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  unsigned int stride;
  int one_hit;
  int cull;
  int ccw;
  int t_arity;
  const ERL_NIF_TERM* tuple;
  double temp;
  float m[16];
  int i;

  if(!enif_get_uint(env, argv[0], &stride)) return enif_make_badarg(env);
  if(!enif_inspect_binary(env, argv[1], &bin)) return enif_make_badarg(env);
  one_hit = enif_is_identical(argv[2], atom_true);
  cull = enif_is_identical(argv[3], atom_true);
  ccw = enif_is_identical(argv[4], atom_true);
  if(!enif_get_tuple(env, argv[5], &t_arity, &tuple) || t_arity != 16)
    return enif_make_badarg(env);
  for(i = 0; i < t_arity; i++) {
    enif_get_double(env, tuple[i], &temp);
    m[i] = (float) temp;
  }

  return pick((float *)bin.data, stride / sizeof(float), bin.size / (stride*3),
              m, ccw, cull, one_hit, env);
}

/*
 * Picking code.
 */

static ERL_NIF_TERM
pick(float* vs, unsigned stride, unsigned num_tris,
     float m[16], int ccw, int cull, int one_hit, ErlNifEnv *env)
{
  unsigned i;

  ERL_NIF_TERM res_array[PRE_RES_SIZE];
  ERL_NIF_TERM* res;
  int res_size = 0;
  int max = PRE_RES_SIZE;

  double last_depth = 42.0;
  int nearest = -1;

  /* printf("\r\nN:stride %d n %d ccw %d cull %d one %d\r\n",
   *  stride, num_tris, ccw, cull, one_hit);
   */

  res = res_array;

  for (i = 0; i < num_tris; i++) {
    /* Storage for triangle vertices follow. We do the clipping
     * in the same buffer, moving forward all the time, never
     * reusing a vertex position. Each clipping plane may add
     * at most one vertex.
     */
    vertex tri[3 + 4+5+6+7+8+9];
    unsigned codes[3];		/* Outcodes for all original triangle vertices */
    unsigned j;

    /*
     * For clipping in the non-trivial case.
     */
    vertex* prev_vp;	       /* Previous vertex */
    vertex* cur_vp;	       /* Current vertex */
    vertex* next_vp;	       /* Where to store the next vertex */
    int vs_left;	       /* Vertices left in the polygon */
    int vs_new;		       /* Number of new and copied vertices in
                                  the clipped polygon */
    int plane;		       /* Number of current plane */
    int reject;		       /* Non-trivial reject boolean */

    /*
     * Pick up the three vertices in a triangle and transform them
     * with the pick matrix. Calculate the outcodes for each vertex.
     */
    for (j = 0; j < 3; j++) {
      unsigned code;
      vertex* vp = tri+j;
      mul(vp, vs[0], vs[1], vs[2], m);
      vs += stride;

      /*
       * We set the bit for each plane if the vertex is outside
       * the plane.
       */
#define OUTCODE(bit, dot) ((dot) < 0.0f ? bit : 0)
      code  = OUTCODE(1, vp->x);
      code |= OUTCODE(2, vp->w - vp->x);
      code |= OUTCODE(4, vp->y);
      code |= OUTCODE(8, vp->w - vp->y);
      code |= OUTCODE(16, vp->z);
      code |= OUTCODE(32, vp->w - vp->z);
      codes[j] = code;
#undef OUTCODE
    }

    /*
     * Trivial reject test. AND the out codes. If the result is
     * non-zero, it means that all vertices are outside at least
     * one of the planes.
     */
    if (codes[0] & codes[1] & codes[2]) {
#if 0
      fprintf(stderr, "%d: Trivial reject\r\n", i);
#endif
      continue;
    }

    /*
     * Trivial accept test. OR the out codes. If the result is
     * zero, all vertices are inside all of the clipping planes.
     * (Only likely to happen for marquee selections.)
     */
    if ((codes[0] | codes[1] | codes[2]) == 0) {
#if 0
      fprintf(stderr, "%d: Trivial accept\r\n", i);
#endif
      if(!cull || do_cull(tri, ccw)) {
        if(one_hit) {
          double depth = tri[0].z / tri[0].w;
          if(depth < last_depth) {
            last_depth = depth;
            nearest = 3*i;
          }
        } else if(res_size < MAX_RES_SIZE) {
          if(res_size == max) {
            /* realloc */
            ERL_NIF_TERM *temp;
            max *= 2;
            temp = enif_alloc(max*sizeof(ERL_NIF_TERM));
            for(j=0; j < res_size; j++) {
              temp[j] = res[j];
            }
            if(res != res_array) {
              enif_free(res);
            }
            res = temp;
          }
          res[res_size++] = enif_make_int(env, 3*i);
        } else {
          /* give up we only handle MAX_RES_SIZE objects */
          break;
        }
      }
      continue;
    }

    /*
     * Start of non-trivial clipping. We must clip the polygon (which
     * starts out as a triangle) against each clipping plane in turn.
     * If we have less than three vertices less after clipping against
     * a plane, we have a non-trivial reject. If the polygon survives
     * all clipping planes we have a non-trivial accept.
     */
    prev_vp = tri+2;
    cur_vp = tri;
    vs_left = 3;
    next_vp = tri + 3;
    vs_new = 0;
    reject = 0;

    for (plane = 0; plane < 6; plane++) {
      while (vs_left-- > 0) {
        float bc_prev;		/* Boundary code for previous  */
        float bc_cur;		/* Boundary code for current */

        switch (plane) {
        case 0:
          bc_prev = prev_vp->x;
          bc_cur = cur_vp->x;
          break;
        case 1:
          bc_prev = prev_vp->w - prev_vp->x;
          bc_cur = cur_vp->w - cur_vp->x;
          break;
        case 2:
          bc_prev = prev_vp->y;
          bc_cur = cur_vp->y;
          break;
        case 3:
          bc_prev = prev_vp->w - prev_vp->y;
          bc_cur = cur_vp->w - cur_vp->y;
          break;
        case 4:
          bc_prev = prev_vp->z;
          bc_cur = cur_vp->z;
          break;
        case 5:
          bc_prev = prev_vp->w - prev_vp->z;
          bc_cur = cur_vp->w - cur_vp->z;
          break;
        default:
          abort();
        }

        if (bc_prev < 0.0f) {
          /* The previous vertex is outside */
          if (bc_cur >= 0.0f) {
            /* The current vertex is inside. We'll add a new vertex
             * at the intersectin point on the clipping plane and
             * we will keep the current vertex.
             */
            intersection(next_vp, prev_vp, cur_vp, bc_prev, bc_cur);
            next_vp++;
            *next_vp++ = *cur_vp;
            vs_new += 2;
          }
          /* Nothing to do if both are outside. */
        } else {
          /* The previous vertex is inside. */
          if (bc_cur < 0.0f) {
            /* The current vertex is outside. Add the intersection
             * with the clipping plane. (Discard current vertex.) */
            intersection(next_vp, prev_vp, cur_vp, bc_prev, bc_cur);
            next_vp++;
            vs_new++;
          } else {
            /* Both vertices are inside. Keep the current vertex. */
            *next_vp++ = *cur_vp;
            vs_new++;
          }
        }
        prev_vp = cur_vp++;
      }

      if (vs_new < 3) {
        /*
         * Too few vertices. No longer a polygon. Non-trivial reject.
         */
        reject = 1;
        break;
      }
      prev_vp = next_vp - 1;
      cur_vp = next_vp - vs_new;
      vs_left = vs_new;
      vs_new = 0;
    }
#if 0
    if (reject) {
      fprintf(stderr, "%d: non-trivial reject\r\n", i);
    } else {
      fprintf(stderr, "%d: non-trivial reject\r\n", i);
    }
#endif
    if (!reject && (!cull || do_cull(cur_vp, ccw))) {
      if(one_hit) {
        double depth = cur_vp[0].z / cur_vp[0].w;
        if(depth < last_depth) {
          last_depth = depth;
          nearest = 3*i;
        }
      } else if(res_size < MAX_RES_SIZE) {
        if(res_size == max) {
          /* realloc */
          ERL_NIF_TERM *temp;
          max *= 2;
          temp = enif_alloc(max*sizeof(ERL_NIF_TERM));
          for(j=0; j < res_size; j++) {
            temp[j] = res[j];
          }
          if(res != res_array) {
            enif_free(res);
          }
          res = temp;
        }
        res[res_size++] = enif_make_int(env, 3*i);
      } else {
        /* give up we only handle MAX_RES_SIZE objects */
        break;
      }
    }
  }

  if(res_size > 0 || nearest > -1) {
    if(one_hit) {
      unsigned depth;
      depth = (unsigned) (last_depth * (double) (0xFFFFFFFF) + 0.5);
      return enif_make_tuple2(env,
                              enif_make_int(env, nearest),
                              enif_make_uint(env, depth));
    } else {
      ERL_NIF_TERM temp;
      temp = enif_make_list_from_array(env, res, res_size);
      if(res != res_array) enif_free(res);
      return temp;
    }
  } else {
    return enif_make_list(env, 0);
  }
}

static void
mul(vertex* out, float x, float y, float z, float m[16])
{
  out->x = m[0]*x + m[4]*y + m[8]*z + m[12];
  out->y = m[1]*x + m[5]*y + m[9]*z + m[13];
  out->z = m[2]*x + m[6]*y + m[10]*z + m[14];
  out->w = m[3]*x + m[7]*y + m[11]*z + m[15];
}

static void
intersection(vertex* out, vertex* prev_vp, vertex* cur_vp,
	     float bc_prev, float bc_cur)
{
  float alpha = bc_prev / (bc_prev - bc_cur);
  out->x = prev_vp->x + alpha * (cur_vp->x - prev_vp->x);
  out->y = prev_vp->y + alpha * (cur_vp->y - prev_vp->y);
  out->z = prev_vp->z + alpha * (cur_vp->z - prev_vp->z);
  out->w = prev_vp->w + alpha * (cur_vp->w - prev_vp->w);
}

static int
do_cull(vertex* vp, int ccw_is_front)
{
  /*
   * Calculate the signed area of the first three vertices
   * (converted to windows coordinates).
   */
  float Dx02, Dx12, Dy02, Dy12;
  float area;
  vp[0].x /= vp[0].w; vp[0].y /= vp[0].w;
  vp[1].x /= vp[1].w; vp[1].y /= vp[1].w;
  vp[2].x /= vp[2].w; vp[2].y /= vp[2].w;
  Dx02 = vp[0].x - vp[2].x;
  Dx12 = vp[1].x - vp[2].x;
  Dy02 = vp[0].y - vp[2].y;
  Dy12 = vp[1].y - vp[2].y;
  area = Dx02*Dy12 - Dx12*Dy02;
  if (area < 0.0f && ccw_is_front) {
    return 0;
  }
  return 1;
}

#if 0
static void
print_tri(vertex* tri)
{
  int j;

  for (j = 0; j < 3; j++) {
    print_vertex(tri+j);
    putchar(' ');
  }
  putchar('\r');
  putchar('\n');
}

static void
print_vertex(vertex* v)
{
  printf("(%f, %f, %f, %f)", v->x, v->y, v->z, v->w);
}
#endif



static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    atom_ok    = enif_make_atom(env,"ok");
    atom_true  = enif_make_atom(env,"true");
    atom_false = enif_make_atom(env,"false");

    // avec_r  = enif_open_resource_type(env, "eblas", "avec", NULL, ERL_NIF_RT_CREATE, NULL);
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

ERL_NIF_INIT(wings_pick_nif,nif_funcs,load,NULL,upgrade,unload)
