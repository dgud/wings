/*
 *  wings_ogla.c --
 *
 *     Erlang driver for OpenGL acceleration.
 *
 *  Copyright (c) 2004-2008 Bjorn Gustavsson
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

#if defined(__APPLE__) && defined(__MACH__)
# include <OpenGL/gl.h>	/* Header File For The OpenGL Library */
# include <OpenGL/glu.h>	/* Header File For The OpenGL Library */
#else
# include <GL/gl.h>	/* Header File For The OpenGL Library */
# include <GL/glu.h>	/* Header File For The OpenGL Library */
#endif

#ifndef CALLBACK
# define CALLBACK
#endif

/*
 * Interface routines.
 */
static ErlDrvData wings_file_start(ErlDrvPort port, char *buff);
static void wings_file_stop(ErlDrvData handle);
static int control(ErlDrvData handle, unsigned int command, 
                   char* buff, int count, 
                   char** res, int res_size);
static int triangulate(char* buff, int count, char** res);

/*
 * Internal routines
 */

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
    "wings_ogla_drv",   /* char *driver_name, the argument to open_port */
    NULL,                  /* F_PTR finish, called when unloaded */
    NULL,                  /* void * that is not used (BC) */
    control,               /* F_PTR control, port_control callback */
    NULL,                  /* F_PTR timeout, driver_set_timer callback */
    NULL                   /* F_PTR outputv, reserved */
};

static GLUtesselator* tess;
static GLdouble* tess_coords;
static GLdouble* tess_alloc_vertex;
static GLdouble* tess_alloc_vertex_end;
static int* tess_vertices;
static int* tess_vertices_end;

void CALLBACK
wings_ogla_vertex(GLdouble* coords)
{
  /*
   * We will simply ignore any vertex indices not fitting in the
   * preallocated buffer. The buffer size should be a multiple of
   * of 3, so that we return only complete triangles.
   */
  if (tess_vertices < tess_vertices_end) {
    *tess_vertices++ = (int) (coords - tess_coords) / 3;
  }
}

void CALLBACK
wings_ogla_edge_flag(GLboolean flag)
{
}

void CALLBACK
wings_ogla_error(GLenum errorCode)
{
   const GLubyte *err;
   err = gluErrorString(errorCode);
   fprintf(stderr, "Tesselation error: %d: %s\r\n", (int)errorCode, err);
}

void CALLBACK
wings_ogla_combine(GLdouble coords[3],
		   void* vertex_data[4],
		   GLfloat w[4], 
		   void **dataOut)
{
  GLdouble* vertex = tess_alloc_vertex;
  int i;

  if (tess_alloc_vertex < tess_alloc_vertex_end) {
    tess_alloc_vertex += 3;
  }

#if 0
  fprintf(stderr, "combine: ");
  for (i = 0; i < 4; i++) {
    if (w[i] > 0.0) {
      fprintf(stderr, "%d(%g) ", (int) vertex_data[i], w[i]);
    }
  }
  fprintf(stderr, "\r\n");
  fprintf(stderr, "%g %g %g\r\n", vertex[0], vertex[1], vertex[2]);
#endif

  vertex[0] = coords[0];
  vertex[1] = coords[1];
  vertex[2] = coords[2];
  *dataOut = vertex;
}

/*
 * Driver initialization routine
 */
DRIVER_INIT(wings_file_drv)
{
  return &wings_file_driver_entry;
}

/*
 * Driver interface routines
 */

/*
 * Open a port
 */
static ErlDrvData wings_file_start(ErlDrvPort port, char *buff)
{
  tess = gluNewTess();
  gluTessCallback(tess, GLU_TESS_VERTEX, wings_ogla_vertex);
  gluTessCallback(tess, GLU_TESS_EDGE_FLAG, wings_ogla_edge_flag);
  gluTessCallback(tess, GLU_TESS_COMBINE, wings_ogla_combine);
  gluTessCallback(tess, GLU_TESS_ERROR, wings_ogla_error);
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
  return (ErlDrvData) 0;
}

/*
 * Close a port
 */
static void wings_file_stop(ErlDrvData handle)
{
  gluDeleteTess(tess);
}

static int
control(ErlDrvData handle, unsigned int command,
        char* buff, int count, 
        char** res, int res_size)
{
  switch (command) {
  case 0: {
    GLfloat* f = (GLfloat *) buff;
    glVertex3fv(f);
    glVertex3fv(f+3);
    glVertex3fv(f+6);
    *res = 0;
    return 0;
  }
  case 1: {
    GLfloat* f = (GLfloat *) buff;
    glVertex3fv(f);
    glVertex3fv(f+3);
    glVertex3fv(f+6);
    glVertex3fv(f+6);
    glVertex3fv(f+9);
    glVertex3fv(f);
    *res = 0;
    return 0;
  }
  case 2: {
    GLfloat* f = (GLfloat *) buff;
    glVertex3fv(f);
    glVertex3fv(f+3);
    glVertex3fv(f+6);
    glVertex3fv(f+9);
    *res = 0;
    return 0;
  }
  case 3: {
    GLfloat* f = (GLfloat *) buff;
    glVertex3fv(f);
    glVertex3fv(f+3);
    *res = 0;
    return 0;
  }
  case 4: {
    GLfloat* f = (GLfloat *) buff;
    return triangulate(buff, count, res);
  }
  default:
    return -1;
  }
}

static int
triangulate(char* buff, int count, char** res)
{
  ErlDrvBinary* bin;
  int i;
  int bin_sz;
  int new_sz;
  int allocated_vertex_indices;
  GLdouble n[3];
  GLdouble* new_vertices;
  int allocated_vertices;
  int num_vertices = count/sizeof(GLdouble)/3 - 1;

  /*
   * Allocate a vertex buffer to fit both all the original
   * vertices, and hopefully any new vertices created.
   * We need to have all vertices in contigous memory so that
   * we easily can calculate a vertex index from a pointer to
   * vertex data.
   */
  allocated_vertices = count + 10*count;
  tess_coords = malloc(allocated_vertices);
  tess_alloc_vertex_end = (GLdouble *) (((char *)tess_coords) +
					allocated_vertices);
  tess_alloc_vertex = new_vertices = tess_coords + count/sizeof(GLdouble);
  memcpy(n, buff, 3*sizeof(GLdouble));
  memcpy(tess_coords, buff, count);
  
  /*
   * Allocate the binary to receive the result. The number of vertex
   * indices must be a multiple of 3, to ensure that we get an integral
   * number of triangles.
   */
  allocated_vertex_indices = 3*6*num_vertices;
  bin = driver_alloc_binary(allocated_vertex_indices*sizeof(int)+sizeof(int));
  tess_vertices = (int *) bin->orig_bytes;
  tess_vertices_end = tess_vertices + allocated_vertex_indices;

  /*
   * Do the triangulation.
   */
  gluTessNormal(tess, n[0], n[1], n[2]);
  gluTessBeginPolygon(tess, 0);
  gluTessBeginContour(tess);
  for (i = 1; i <= num_vertices; i++) {
    gluTessVertex(tess, tess_coords+3*i, tess_coords+3*i);
  }
  gluTessEndContour(tess);
  gluTessEndPolygon(tess);

  /*
   * Test for vertex buffer overflow. Return a fake triangulation
   * if there was an overflow.
   */
  if (!(tess_alloc_vertex < tess_alloc_vertex_end)) {
    tess_vertices = (int *) bin->orig_bytes;
    *tess_vertices++ = 1;
    *tess_vertices++ = 2;
    *tess_vertices++ = 3;
    tess_alloc_vertex = new_vertices;
  }

  /*
   * Finish the list of vertex indices with an invalid index (0).
   */
  *tess_vertices++ = 0;

  /*
   * Reallocate the binary to the exact size of the data to return. If
   * any new vertices have been created, they will be returned after
   * the the list of vertex indices.
   */
  new_sz = (tess_alloc_vertex - new_vertices)*sizeof(GLdouble);
  bin_sz = ((char *)tess_vertices) - bin->orig_bytes;
  bin = driver_realloc_binary(bin, bin_sz + new_sz);
  *res = (char *) bin;
  tess_vertices = (int *) (bin->orig_bytes + bin_sz);
  if (new_sz != 0) {
    memcpy(tess_vertices, new_vertices, new_sz);
  }

  free(tess_coords);
  return 0;
}
