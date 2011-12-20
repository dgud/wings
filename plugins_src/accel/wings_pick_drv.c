/*
 *  wings_ogla.c --
 *
 *     Erlang driver for picking.
 *
 *  Copyright (c) 2009-2010 Bjorn Gustavsson
 *
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id: wings_ogla_drv.c,v 1.3 2004/04/20 18:14:22 bjorng Exp $
 */

#include <stdio.h>
#include "erl_driver.h"

#ifdef __WIN32__
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif
#include <string.h>

#if (ERL_DRV_EXTENDED_MAJOR_VERSION < 2)
/* R14B or earlier types */
#define ErlDrvSizeT  int
#define ErlDrvSSizeT int
#endif

/*
 * Interface routines.
 */
static ErlDrvData wings_file_start(ErlDrvPort port, char *buff);
static void wings_file_stop(ErlDrvData handle);
static ErlDrvSSizeT control(ErlDrvData handle, unsigned int command,
			    char* buff, ErlDrvSizeT count,
			    char** res, ErlDrvSizeT res_size);
static void outputv(ErlDrvData drv_data, ErlIOVec* ev);

/*
 * The driver struct
 */
ErlDrvEntry wings_file_driver_entry = {
    NULL,		   /* F_PTR init, N/A */
    wings_file_start,      /* L_PTR start, called when port is opened */
    wings_file_stop,       /* F_PTR stop, called when port is closed */
    NULL,                  /* F_PTR output, called when erlang has sent */
    NULL,                  /* F_PTR ready_input, called when input descriptor
			      ready */
    NULL,                  /* F_PTR ready_output, called when output
			      descriptor ready */
    "wings_pick_drv",      /* char *driver_name, the argument to open_port */
    NULL,                  /* F_PTR finish, called when unloaded */
    NULL,                  /* void * that is not used (BC) */
    control,               /* F_PTR control, port_control callback */
    NULL,                  /* F_PTR timeout, driver_set_timer callback */
    outputv,               /* F_PTR outputv, reserved */
    NULL,                  /* async */ 
    NULL,                  /* flush */
    NULL,                  /* call */
    NULL,                  /* Event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    ERL_DRV_FLAG_USE_PORT_LOCKING, /* Port lock */ 
    NULL,                  /* Reserved Handle */
    NULL,                  /* Process Exited */
};

struct vertex_struct {
  float x, y, w, z;
};
typedef struct vertex_struct vertex;

/*
 * Driver data (non-reetrant, non-thread safe).
 */
#define MAX_RES_SIZE (1024*1204)
static float m[16];
static ErlDrvBinary* result;
static unsigned res_size;
static int cull = 1;
static int ccw_is_front = 1;
static int one_hit = 1;
static double last_depth;

/* Declarations of internal functions */
static void pick(float* vertices, unsigned stride, unsigned num_tris);
static void mul(vertex* out, float x, float y, float z);
static void intersection(vertex* out, vertex* prev_vp, vertex* cur_vp,
			 float bc_prev, float bc_cur);
static void do_accept(unsigned i, vertex* vp);
#if 0
static void print_tri(vertex* tri);
static void print_vertex(vertex* v);
#endif

/*
 * Driver initialization routine
 */
DRIVER_INIT(wings_file_drv)
{
  return &wings_file_driver_entry;
}

/*
 * Open a port
 */
static ErlDrvData wings_file_start(ErlDrvPort port, char *buff)
{
  result = driver_alloc_binary(MAX_RES_SIZE);
  return (ErlDrvData) port;
}

/*
 * Close a port
 */
static void wings_file_stop(ErlDrvData handle)
{
  /* Nothing to do yet. */
}

/*
 * Handle commands.
 */

static ErlDrvSSizeT
control(ErlDrvData handle, unsigned int command,
        char* buf, ErlDrvSizeT count,
        char** res, ErlDrvSizeT res_size)
{
  switch (command) {
  case 0: {			/* Define matrix */
      memcpy((void *) m, (void *) buf, count);
#if 0
    {
      int i, j;
      for (i = 0; i < 4; i++) {
	for (j = 0; j < 4; j++) {
	  printf("%f ", m[i][j]);
	}
	printf("\r\n");
      }
      printf("\r\n\r\n");
    }
#endif
    return 0;
  }
  case 1: {			/* Enable/disable culling */
    cull = buf[0];
    return 0;
  }
  case 2: {			/* Define front-facing faces */
    ccw_is_front = buf[0];
    return 0;
  }
  case 3: {
    one_hit = buf[0];		/* One hit (if non-zero) or all hits */
    return 0;
  }
  default:
    return -1;
  }
}

static void
outputv(ErlDrvData drv_data, ErlIOVec* ev)
{
  ErlDrvPort port = (ErlDrvPort) drv_data;

  if (ev->vsize == 3 && ev->iov[1].iov_len == 4) {
    unsigned stride = *(unsigned *) ev->iov[1].iov_base;
    float* base = (float *) ev->iov[2].iov_base;
    unsigned n = ev->iov[2].iov_len / stride / 3;

    pick(base, stride / sizeof(float), n);
    driver_output_binary(port, 0, 0, result, 0, res_size);
  }
}

/*
 * Picking code.
 */

static void
pick(float* vs, unsigned stride, unsigned num_tris)
{
  unsigned i;

  res_size = 0;
  last_depth = 42.0;
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
      mul(vp, vs[0], vs[1], vs[2]);
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
      do_accept(3*i, tri);
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
    if (!reject) {
      do_accept(3*i, cur_vp);
    }
  }
}

static void
mul(vertex* out, float x, float y, float z)
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

static void
do_accept(unsigned i, vertex* vp)
{
  if (cull) {
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
      return;
    }
  }

  if (one_hit) {
    double depth = vp[0].z / vp[0].w;
    if (depth < last_depth) {
      unsigned *p = (unsigned *) result->orig_bytes;
      *p++ = i;
      *p++ = (unsigned) (depth * (double) (0xFFFFFFFF) + 0.5);
      res_size = 2*sizeof(unsigned);
      last_depth = depth;
    }
  } else if (res_size < MAX_RES_SIZE) {
    unsigned *p = (unsigned *) (result->orig_bytes+res_size);
    *p = i;
    res_size += sizeof(unsigned);
  }
}

#if 0
static void
print_tri(vertex* tri)
{
  int i, j;

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

