/*
 *  wings_ogla.c --
 *
 *     Erlang driver for OpenGL acceleration.
 *
 *  Copyright (c) 2004 Bjorn Gustavsson
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
static int* tess_vertices;

void CALLBACK
wings_ogla_vertex(GLdouble* coords)
{
  /* fprintf(stderr, "%d\r\n", (int) (coords - tess_coords) / 3); */

  *tess_vertices++ = (int) (coords - tess_coords) / 3;
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

  tess_alloc_vertex += 3;

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
  int new_sz;
  int bin_sz;
  GLdouble n[3];
  GLdouble* new_vertices;
  int num_vertices = count/sizeof(GLdouble)/3 - 1;

  tess_coords = malloc(6*count);
  tess_alloc_vertex = new_vertices = tess_coords + count/sizeof(GLdouble);

#if 0
  fprintf(stderr, "n=%d\r\n", num_vertices);
#endif
  bin = driver_alloc_binary(16*num_vertices*sizeof(int));
  *res = (char *) bin;
  tess_vertices = (int *) bin->orig_bytes;

  memcpy(n, buff, 3*sizeof(GLdouble));
  memcpy(tess_coords, buff, count);

  gluTessNormal(tess, n[0], n[1], n[2]);
  gluTessBeginPolygon(tess, 0);
  gluTessBeginContour(tess);
  for (i = 1; i <= num_vertices; i++) {
    gluTessVertex(tess, tess_coords+3*i, tess_coords+3*i);
  }
  gluTessEndContour(tess);
  gluTessEndPolygon(tess);
  *tess_vertices++ = 0;

  new_sz = (tess_alloc_vertex-new_vertices)*sizeof(GLdouble);
  bin_sz = (((char *)tess_vertices) - bin->orig_bytes) + new_sz;
  driver_realloc_binary(bin, bin_sz);

  if (new_sz != 0) {
    memcpy(tess_vertices, new_vertices, new_sz);
  }

  free(tess_coords);
  return 0;
}
