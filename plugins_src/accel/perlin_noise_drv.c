/*
 *  perlin_noise_drv.c --
 *
 *     Erlang driver for generating simplex perlin noise
 *
 *  Copyright (c) 2005-2010 Dan Gudmundsson
 *
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id: perlin_noise_drv.c,v 1.4 2006/04/27 13:46:55 dgud Exp $
 */

#include <stdio.h>
#include "erl_driver.h"

#ifdef __WIN32__
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#if (ERL_DRV_EXTENDED_MAJOR_VERSION < 2)
/* R14B or earlier types */
#define ErlDrvSizeT  int
#define ErlDrvSSizeT int
#endif

#include <math.h>
#include <string.h>

#define PNOISE3 3
#define SNOISE1 4
#define SNOISE2 5
#define SNOISE3 6
#define SNOISE4 7

#define PNOISE_MAP1 11
#define PNOISE_MAP2 12
#define PNOISE_MAP3 13

#define SNOISE_MAP1 14
#define SNOISE_MAP2 15
#define SNOISE_MAP3 16

/* Function declarations */
double   fade(double t);
double   lerp(double t, double a, double b);
double   grad(int hash, double x, double y, double z); 
void     init();
double   pnoise(double x, double y, double z);

/** 1D, 2D, 3D and 4D float Perlin noise
 */
double snoise1( double x );
double snoise2( double x, double y );
double snoise3( double x, double y, double z );
double snoise4( double x, double y, double z, double w );

#define to_byte(X) (((X)+1.0)*128.0)  /* from -1,1 => 0 - 255 */

/*
 * Interface routines.
 */
static ErlDrvData perlin_noise_start(ErlDrvPort port, char *buff);
static void perlin_noise_stop(ErlDrvData handle);
static ErlDrvSSizeT control(ErlDrvData handle, unsigned int command, 
			    char* buff, ErlDrvSizeT count, 
			    char** res, ErlDrvSizeT res_size);

/*
 * Internal routines
 */

/*
 * The driver struct
 */
ErlDrvEntry perlin_file_driver_entry = {
   NULL,		   /* F_PTR init, N/A */
   perlin_noise_start,      /* L_PTR start, called when port is opened */
   perlin_noise_stop,       /* F_PTR stop, called when port is closed */
   NULL,                  /* F_PTR output, called when erlang has sent */
   NULL,                  /* F_PTR ready_input, called when input descriptor 
			     ready */
   NULL,                  /* F_PTR ready_output, called when output 
			     descriptor ready */
   "perlin_noise_drv",     /* char *driver_name, the argument to open_port */
   NULL,                  /* F_PTR finish, called when unloaded */
   NULL,                  /* void * that is not used (BC) */
   control,               /* F_PTR control, port_control callback */
   NULL,                  /* F_PTR timeout, driver_set_timer callback */
   NULL,                  /* F_PTR outputv, reserved */
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

/*
 * Driver initialization routine
 */
DRIVER_INIT(perlin_file_drv)
{
   return &perlin_file_driver_entry;
}

/*
 * Driver interface routines
 */

/*
 * Open a port
 */
static ErlDrvData perlin_noise_start(ErlDrvPort port, char *buff)
{
   set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
   return (ErlDrvData) 0;
}

/*
 * Close a port
 */
static void perlin_noise_stop(ErlDrvData handle)
{
   
}

static ErlDrvSSizeT control(ErlDrvData handle, unsigned int command,
			    char* buff, ErlDrvSizeT count, 
			    char** res, ErlDrvSizeT res_size)
{
   ErlDrvBinary* bin;

   switch (command) {

   case PNOISE3: {
      double f[3], *ptr;
      memcpy(f, buff, sizeof(double)*3);
      bin = driver_alloc_binary(sizeof(double)); 
      ptr = (double *) bin->orig_bytes;
      *ptr = pnoise(f[0], f[1], f[2]);
      *res = (char *) bin;
      return sizeof(double);
   }

   case SNOISE1: {
      double f[1], *ptr;
      memcpy(f, buff, sizeof(double)*1);
      bin = driver_alloc_binary(sizeof(double)); 
      ptr = (double *) bin->orig_bytes;
      *ptr = snoise1(f[0]);
      *res = (char *) bin;
      return sizeof(double);
   }
   case SNOISE2: {
      double f[2], *ptr;
      memcpy(f, buff, sizeof(double)*2);
      bin = driver_alloc_binary(sizeof(double)); 
      ptr = (double *) bin->orig_bytes;
      *ptr = snoise2(f[0], f[1]);
      *res = (char *) bin;
      return sizeof(double);
   }
   case SNOISE3: {
       double f[3], *ptr;
      memcpy(f, buff, sizeof(double)*3);
      bin = driver_alloc_binary(sizeof(double)); 
      ptr = (double *) bin->orig_bytes;
      *ptr = snoise3(f[0], f[1], f[2]);
      *res = (char *) bin;
      return sizeof(double);
   }
   case SNOISE4: {
      double f[4], *ptr;
      memcpy(f, buff, sizeof(double)*4);
      bin = driver_alloc_binary(sizeof(double)); 
      ptr = (double *) bin->orig_bytes;
      *ptr = snoise4(f[0], f[1], f[2], f[3]);
      *res = (char *) bin;
      return sizeof(double);
   }
      /* */ 
   case PNOISE_MAP1: {
      int i;
      int sz = * ((unsigned int*) buff);
      unsigned char *noise;
      bin = driver_alloc_binary(sz); 
      noise = (unsigned char*) bin->orig_bytes;
      for(i=0; i < sz ; i++) {
	 double iv = (double)i/(sz-1);

	 * noise++ = (unsigned char) to_byte(pnoise(iv,iv,iv));
	 * noise++ = (unsigned char) to_byte(pnoise(iv*2.0,iv*2.0,iv*2.0));
	 * noise++ = (unsigned char) to_byte(pnoise(iv*4.0,iv*4.0,iv*4.0));
	 * noise++ = (unsigned char) to_byte(pnoise(iv*8.0,iv*8.0,iv*8.0));
      }
      
      *res = (char *) bin;
      return sz;
   }
   case PNOISE_MAP2: {
      int sz = * ((unsigned int*) buff);
      int i,j;
      unsigned char *noise;
      bin = driver_alloc_binary(sz*sz); 
      noise = (unsigned char*) bin->orig_bytes;
      
      for(i=0; i < sz ; i++) {
	 for(j=0; j < sz ; j++) {
	    double 
	       iv = (double)i/(sz-1),
	       jv = (double)j/(sz-1),
	       kv = (double)i+j/(sz-1);

	    * noise++ = (unsigned char) to_byte(pnoise(iv,jv,kv));
	    * noise++ = (unsigned char) to_byte(pnoise(iv*2.0,jv*2.0,kv*2.0));
	    * noise++ = (unsigned char) to_byte(pnoise(iv*4.0,jv*4.0,kv*4.0));
	    * noise++ = (unsigned char) to_byte(pnoise(iv*8.0,jv*8.0,kv*8.0));
	 }
      }
      
      *res = (char *) bin;
      return sz*sz;
   }

   case PNOISE_MAP3: {
      int sz = * ((unsigned int*) buff);
      int i,j,k;
      unsigned char *noise;
      bin = driver_alloc_binary(sz*sz*sz); 
      noise = (unsigned char*) bin->orig_bytes;
      
      for(i=0; i < sz ; i++) {
	 for(j=0; j < sz ; j++) {
	    for(k=0; k < sz ; k++) {		
	       double 
		  iv = (double)i/(sz-1),
		  jv = (double)j/(sz-1),
		  kv = (double)k/(sz-1);
	       * noise++ = (unsigned char) to_byte(pnoise(iv,jv,kv));
	       * noise++ = (unsigned char) to_byte(pnoise(iv*2.0,jv*2.0,kv*2.0));
	       * noise++ = (unsigned char) to_byte(pnoise(iv*4.0,jv*4.0,kv*4.0));
	       * noise++ = (unsigned char) to_byte(pnoise(iv*8.0,jv*8.0,kv*8.0));
	    }
	 } 
      }	 
      *res = (char *) bin;
      return sz*sz*sz;
   }

   case SNOISE_MAP1: {
      int i;
      int sz = * ((unsigned int*) buff);
      unsigned char *noise;
      bin = driver_alloc_binary(sz); 
      noise = (unsigned char*) bin->orig_bytes;
      for(i=0; i < sz ; i++) {
	 double 
	    iv = (double)i/(sz-1);
	 * noise++ = (unsigned char) to_byte(snoise1(iv));
	 * noise++ = (unsigned char) to_byte(snoise1(iv*2.0));
	 * noise++ = (unsigned char) to_byte(snoise1(iv*4.0));
	 * noise++ = (unsigned char) to_byte(snoise1(iv*8.0));
      }
      
      *res = (char *) bin;
      return sz;
   }
   case SNOISE_MAP2: {
      int sz = * ((unsigned int*) buff);
      int i,j;
      unsigned char *noise;
      bin = driver_alloc_binary(sz*sz*4); 
      noise = (unsigned char*) bin->orig_bytes;
      
      for(i=0; i < sz ; i++) {
	 for(j=0; j < sz ; j++) {
	    double 
	       iv = (double)i/(sz-1),
	       jv = (double)j/(sz-1);
	    * noise++ = (unsigned char) to_byte(snoise2(iv,jv));
	    * noise++ = (unsigned char) to_byte(snoise2(iv*2.0,jv*2.0));
	    * noise++ = (unsigned char) to_byte(snoise2(iv*4.0,jv*4.0));
	    * noise++ = (unsigned char) to_byte(snoise2(iv*8.0,jv*8.0));
	 }
      }
      
      *res = (char *) bin;
      return sz*sz;
   }

   case SNOISE_MAP3: {
      int sz = * ((unsigned int*) buff);
      int i,j,k;
      unsigned char *noise;

      bin = driver_alloc_binary(sz*sz*sz*4); 
      noise = (unsigned char*) bin->orig_bytes;
      
      for(i=0; i < sz ; i++) {
	  double iv = (double)i/(sz-1);	  
	  for(j=0; j < sz ; j++) {
	      double jv = (double)j/(sz-1);
	      for(k=0; k < sz ; k++) {
		  double kv = (double)k/(sz-1);
		  * noise++ = (unsigned char) to_byte(snoise3(iv,jv,kv));
		  * noise++ = (unsigned char) to_byte(snoise3(iv*2.0,jv*2.0,kv*2.0));
		  * noise++ = (unsigned char) to_byte(snoise3(iv*4.0,jv*4.0,kv*4.0));
		  * noise++ = (unsigned char) to_byte(snoise3(iv*8.0,jv*8.0,kv*8.0));
	      }
	  }
      }
      *res = (char *) bin;
      return sz*sz*sz*4;
   }
      
   default:
      fprintf(stderr, "ARRG whats happening\r\n");
      *res = 0;
      return -1;
   }
}

static int p[512] = 
 { 151,160,137,91,90,15,
   131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,
   21,10,23,190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,
   35,11,32,57,177,33,88,237,149,56,87,174,20,125,136,171,168, 68,175,
   74,165,71,134,139,48,27,166,77,146,158,231,83,111,229,122,60,211,133,
   230,220,105,92,41,55,46,245,40,244,102,143,54, 65,25,63,161, 1,216,
   80,73,209,76,132,187,208, 89,18,169,200,196,135,130,116,188,159,86,
   164,100,109,198,173,186, 3,64,52,217,226,250,124,123,5,202,38,147,
   118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,223,
   183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,
   172,9,129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,
   218,246,97,228,251,34,242,193,238,210,144,12,191,179,162,241, 81,51,
   145,235,249,14,239,107,49,192,214,31,181,199,106,157,184, 84,204,176,
   115,121,50,45,127, 4,150,254,138,236,205,93,222,114,67,29,24,72,243,
   141,128,195,78,66,215,61,156,180,
   151,160,137,91,90,15,
   131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,
   21,10,23,190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,
   35,11,32,57,177,33,88,237,149,56,87,174,20,125,136,171,168, 68,175,
   74,165,71,134,139,48,27,166,77,146,158,231,83,111,229,122,60,211,133,
   230,220,105,92,41,55,46,245,40,244,102,143,54, 65,25,63,161, 1,216,
   80,73,209,76,132,187,208, 89,18,169,200,196,135,130,116,188,159,86,
   164,100,109,198,173,186, 3,64,52,217,226,250,124,123,5,202,38,147,
   118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,223,
   183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,
   172,9,129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,
   218,246,97,228,251,34,242,193,238,210,144,12,191,179,162,241, 81,51,
   145,235,249,14,239,107,49,192,214,31,181,199,106,157,184, 84,204,176,
   115,121,50,45,127, 4,150,254,138,236,205,93,222,114,67,29,24,72,243,
   141,128,195,78,66,215,61,156,180
 };

double fade(double t){ return t * t * t * (t * (t * 6 - 15) + 10); }
double lerp(double t, double a, double b){ return a + t * (b - a); }
double  grad3( int hash, double x, double y , double z ) {
   int h = hash & 15;     // Convert low 4 bits of hash code into 12 simple
   double u = h<8 ? x : y; // gradient directions, and compute dot product.
   double v = h<4 ? y : h==12||h==14 ? x : z; // Fix repeats at h = 12 to 15
   return ((h&1)? -u : u) + ((h&2)? -v : v);
}

double pnoise(double x, double y, double z) 
{
   int   X,Y,Z;
   double u,v,w;
   int A,AA,AB,B,BA,BB;

   X = (int)floor(x) & 255;             /* FIND UNIT CUBE THAT */
   Y = (int)floor(y) & 255;             /* CONTAINS POINT.     */
   Z = (int)floor(z) & 255;
   x -= floor(x);                       /* FIND RELATIVE X,Y,Z */
   y -= floor(y);                       /* OF POINT IN CUBE.   */
   z -= floor(z);
   u = fade(x);                         /* COMPUTE FADE CURVES */
   v = fade(y);                         /* FOR EACH OF X,Y,Z.  */
   w = fade(z);

   A  = p[X]+Y;
   AA = p[A]+Z;
   AB = p[A+1]+Z; /* HASH COORDINATES OF */
   B  = p[X+1]+Y;
   BA = p[B]+Z;
   BB = p[B+1]+Z; /* THE 8 CUBE CORNERS, */

   return lerp(w,lerp(v,lerp(u, grad3(p[AA  ], x, y, z),    /* AND ADD */
			     grad3(p[BA  ], x-1, y, z)),    /* BLENDED */
		      lerp(u, grad3(p[AB  ], x, y-1, z),    /* RESULTS */
			   grad3(p[BB  ], x-1, y-1, z))),   /* FROM  8 */
	       lerp(v, lerp(u, grad3(p[AA+1], x, y, z-1 ),  /* CORNERS */
			    grad3(p[BA+1], x-1, y, z-1)),   /* OF CUBE */
		    lerp(u, grad3(p[AB+1], x, y-1, z-1),
			 grad3(p[BB+1], x-1, y-1, z-1))));
}


// SimplexNoise1234
// Copyright © 2003-2005, Stefan Gustavson
//
// Contact: stegu@itn.liu.se
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation; either
// version 2 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#define FASTFLOOR(x) ( ((x)>0) ? ((int)x) : (((int)x)-1) )

/*
 * Helper functions to compute gradients-dot-residualvectors (1D to 4D)
 * Note that these generate gradients of more than unit length. To make
 * a close match with the value range of classic Perlin noise, the final
 * noise values need to be rescaled to fit nicely within [-1,1].
 * (The simplex noise functions as such also have different scaling.)
 * Note also that these noise functions are the most practical and useful
 * signed version of Perlin noise. To return values according to the
 * RenderMan specification from the SL noise() and pnoise() functions,
 * the noise values need to be scaled and offset to [0,1], like this:
 * float SLnoise = (SimplexNoise1234::noise(x,y,z) + 1.0) * 0.5;
 */

double  grad1( int hash, double x ) {
   int h = hash & 15;
   double grad = 1.0f + (h & 7);   // Gradient value 1.0, 2.0, ..., 8.0
   if (h&8) grad = -grad;         // Set a random sign for the gradient
   return ( grad * x );           // Multiply the gradient with the distance
}

double  grad2( int hash, double x, double y ) {
   int h = hash & 7;      // Convert low 3 bits of hash code
   double u = h<4 ? x : y;  // into 8 simple gradient directions,
   double v = h<4 ? y : x;  // and compute the dot product with (x,y).
   return ((h&1)? -u : u) + ((h&2)? -2.0f*v : 2.0f*v);
}

double  grad4( int hash, double x, double y, double z, double t ) {
   int h = hash & 31;      // Convert low 5 bits of hash code into 32 simple
   double u = h<24 ? x : y; // gradient directions, and compute dot product.
   double v = h<16 ? y : z;
   double w = h<8 ? z : t;
   return ((h&1)? -u : u) + ((h&2)? -v : v) + ((h&4)? -w : w);
}

// A lookup table to traverse the simplex around a given point in 4D.
// Details can be found where this table is used, in the 4D noise method.
/* TODO: This should not be required, backport it from Bill's GLSL code! */
static unsigned char simplex[64][4] = {
   {0,1,2,3},{0,1,3,2},{0,0,0,0},{0,2,3,1},{0,0,0,0},{0,0,0,0},{0,0,0,0},{1,2,3,0},
   {0,2,1,3},{0,0,0,0},{0,3,1,2},{0,3,2,1},{0,0,0,0},{0,0,0,0},{0,0,0,0},{1,3,2,0},
   {0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0},
   {1,2,0,3},{0,0,0,0},{1,3,0,2},{0,0,0,0},{0,0,0,0},{0,0,0,0},{2,3,0,1},{2,3,1,0},
   {1,0,2,3},{1,0,3,2},{0,0,0,0},{0,0,0,0},{0,0,0,0},{2,0,3,1},{0,0,0,0},{2,1,3,0},
   {0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0},{0,0,0,0},
   {2,0,1,3},{0,0,0,0},{0,0,0,0},{0,0,0,0},{3,0,1,2},{3,0,2,1},{0,0,0,0},{3,1,2,0},
   {2,1,0,3},{0,0,0,0},{0,0,0,0},{0,0,0,0},{3,1,0,2},{0,0,0,0},{3,2,0,1},{3,2,1,0}};

// 1D simplex noise
double snoise1(double x) {

   int i0 = FASTFLOOR(x);
   int i1 = i0 + 1;
   double x0 = x - i0;
   double x1 = x0 - 1.0f;

   double n0, n1;

   double t0 = 1.0f - x0*x0;
   //  if(t0 < 0.0f) t0 = 0.0f; // this never happens for the 1D case
   t0 *= t0;
   n0 = t0 * t0 * grad1(p[i0 & 0xff], x0);

   double t1 = 1.0f - x1*x1;
   //  if(t1 < 0.0f) t1 = 0.0f; // this never happens for the 1D case
   t1 *= t1;
   n1 = t1 * t1 * grad1(p[i1 & 0xff], x1);
   // The maximum value of this noise is 8*(3/4)^4 = 2.53125
   // A factor of 0.395 would scale to fit exactly within [-1,1], but
   // we want to match PRMan's 1D noise, so we scale it down some more.
   return 0.25f * (n0 + n1);

}

// 2D simplex noise
double snoise2(double x, double y) {

#define F2 0.366025403 // F2 = 0.5*(sqrt(3.0)-1.0)
#define G2 0.211324865 // G2 = (3.0-Math.sqrt(3.0))/6.0

   double n0, n1, n2; // Noise contributions from the three corners

   // Skew the input space to determine which simplex cell we're in
   double s = (x+y)*F2; // Hairy factor for 2D
   double xs = x + s;
   double ys = y + s;
   int i = FASTFLOOR(xs);
   int j = FASTFLOOR(ys);

   double t = (double)(i+j)*G2;
   double X0 = i-t; // Unskew the cell origin back to (x,y) space
   double Y0 = j-t;
   double x0 = x-X0; // The x,y distances from the cell origin
   double y0 = y-Y0;

   // For the 2D case, the simplex shape is an equilateral triangle.
   // Determine which simplex we are in.
   int i1, j1; // Offsets for second (middle) corner of simplex in (i,j) coords
   if(x0>y0) {i1=1; j1=0;} // lower triangle, XY order: (0,0)->(1,0)->(1,1)
   else {i1=0; j1=1;}      // upper triangle, YX order: (0,0)->(0,1)->(1,1)

   // A step of (1,0) in (i,j) means a step of (1-c,-c) in (x,y), and
   // a step of (0,1) in (i,j) means a step of (-c,1-c) in (x,y), where
   // c = (3-sqrt(3))/6

   double x1 = x0 - i1 + G2; // Offsets for middle corner in (x,y) unskewed coords
   double y1 = y0 - j1 + G2;
   double x2 = x0 - 1.0f + 2.0f * G2; // Offsets for last corner in (x,y) unskewed coords
   double y2 = y0 - 1.0f + 2.0f * G2;

   // Wrap the integer indices at 256, to avoid indexing p[] out of bounds
   int ii = i % 256;
   int jj = j % 256;

   // Calculate the contribution from the three corners
   double t0 = 0.5f - x0*x0-y0*y0;
   if(t0 < 0.0f) n0 = 0.0f;
   else {
      t0 *= t0;
      n0 = t0 * t0 * grad2(p[ii+p[jj]], x0, y0); 
   }

   double t1 = 0.5f - x1*x1-y1*y1;
   if(t1 < 0.0f) n1 = 0.0f;
   else {
      t1 *= t1;
      n1 = t1 * t1 * grad2(p[ii+i1+p[jj+j1]], x1, y1);
   }

   double t2 = 0.5f - x2*x2-y2*y2;
   if(t2 < 0.0f) n2 = 0.0f;
   else {
      t2 *= t2;
      n2 = t2 * t2 * grad2(p[ii+1+p[jj+1]], x2, y2);
   }

   // Add contributions from each corner to get the final noise value.
   // The result is scaled to return values in the interval [-1,1].
   return 40.0f * (n0 + n1 + n2); // TODO: The scale factor is preliminary!
}

// 3D simplex noise
double snoise3(double x, double y, double z) {

   // Simple skewing factors for the 3D case
#define F3 0.333333333
#define G3 0.166666667

   double n0, n1, n2, n3; // Noise contributions from the four corners

   // Skew the input space to determine which simplex cell we're in
   double s = (x+y+z)*F3; // Very nice and simple skew factor for 3D
   double xs = x+s;
   double ys = y+s;
   double zs = z+s;
   int i = FASTFLOOR(xs);
   int j = FASTFLOOR(ys);
   int k = FASTFLOOR(zs);

   double t = (double)(i+j+k)*G3; 
   double X0 = i-t; // Unskew the cell origin back to (x,y,z) space
   double Y0 = j-t;
   double Z0 = k-t;
   double x0 = x-X0; // The x,y,z distances from the cell origin
   double y0 = y-Y0;
   double z0 = z-Z0;

   // For the 3D case, the simplex shape is a slightly irregular tetrahedron.
   // Determine which simplex we are in.
   int i1, j1, k1; // Offsets for second corner of simplex in (i,j,k) coords
   int i2, j2, k2; // Offsets for third corner of simplex in (i,j,k) coords

   /* This code would benefit from a backport from the GLSL version! */
   if(x0>=y0) {
      if(y0>=z0)
      { i1=1; j1=0; k1=0; i2=1; j2=1; k2=0; } // X Y Z order
      else if(x0>=z0) { i1=1; j1=0; k1=0; i2=1; j2=0; k2=1; } // X Z Y order
      else { i1=0; j1=0; k1=1; i2=1; j2=0; k2=1; } // Z X Y order
   }
   else { // x0<y0
      if(y0<z0) { i1=0; j1=0; k1=1; i2=0; j2=1; k2=1; } // Z Y X order
      else if(x0<z0) { i1=0; j1=1; k1=0; i2=0; j2=1; k2=1; } // Y Z X order
      else { i1=0; j1=1; k1=0; i2=1; j2=1; k2=0; } // Y X Z order
   }

   // A step of (1,0,0) in (i,j,k) means a step of (1-c,-c,-c) in (x,y,z),
   // a step of (0,1,0) in (i,j,k) means a step of (-c,1-c,-c) in (x,y,z), and
   // a step of (0,0,1) in (i,j,k) means a step of (-c,-c,1-c) in (x,y,z), where
   // c = 1/6.

   double x1 = x0 - i1 + G3; // Offsets for second corner in (x,y,z) coords
   double y1 = y0 - j1 + G3;
   double z1 = z0 - k1 + G3;
   double x2 = x0 - i2 + 2.0f*G3; // Offsets for third corner in (x,y,z) coords
   double y2 = y0 - j2 + 2.0f*G3;
   double z2 = z0 - k2 + 2.0f*G3;
   double x3 = x0 - 1.0f + 3.0f*G3; // Offsets for last corner in (x,y,z) coords
   double y3 = y0 - 1.0f + 3.0f*G3;
   double z3 = z0 - 1.0f + 3.0f*G3;

   // Wrap the integer indices at 256, to avoid indexing p[] out of bounds
   int ii = i % 256;
   int jj = j % 256;
   int kk = k % 256;

   // Calculate the contribution from the four corners
   double t0 = 0.6f - x0*x0 - y0*y0 - z0*z0;
   if(t0 < 0.0f) n0 = 0.0f;
   else {
      t0 *= t0;
      n0 = t0 * t0 * grad3(p[ii+p[jj+p[kk]]], x0, y0, z0);
   }

   double t1 = 0.6f - x1*x1 - y1*y1 - z1*z1;
   if(t1 < 0.0f) n1 = 0.0f;
   else {
      t1 *= t1;
      n1 = t1 * t1 * grad3(p[ii+i1+p[jj+j1+p[kk+k1]]], x1, y1, z1);
   }

   double t2 = 0.6f - x2*x2 - y2*y2 - z2*z2;
   if(t2 < 0.0f) n2 = 0.0f;
   else {
      t2 *= t2;
      n2 = t2 * t2 * grad3(p[ii+i2+p[jj+j2+p[kk+k2]]], x2, y2, z2);
   }

   double t3 = 0.6f - x3*x3 - y3*y3 - z3*z3;
   if(t3<0.0f) n3 = 0.0f;
   else {
      t3 *= t3;
      n3 = t3 * t3 * grad3(p[ii+1+p[jj+1+p[kk+1]]], x3, y3, z3);
   }

   // Add contributions from each corner to get the final noise value.
   // The result is scaled to stay just inside [-1,1]
   return 32.0f * (n0 + n1 + n2 + n3); // TODO: The scale factor is preliminary!
}


// 4D simplex noise
double snoise4(double x, double y, double z, double w) {
  
   // The skewing and unskewing factors are hairy again for the 4D case
#define F4 0.309016994 // F4 = (Math.sqrt(5.0)-1.0)/4.0
#define G4 0.138196601 // G4 = (5.0-Math.sqrt(5.0))/20.0

   double n0, n1, n2, n3, n4; // Noise contributions from the five corners

   // Skew the (x,y,z,w) space to determine which cell of 24 simplices we're in
   double s = (x + y + z + w) * F4; // Factor for 4D skewing
   double xs = x + s;
   double ys = y + s;
   double zs = z + s;
   double ws = w + s;
   int i = FASTFLOOR(xs);
   int j = FASTFLOOR(ys);
   int k = FASTFLOOR(zs);
   int l = FASTFLOOR(ws);

   double t = (i + j + k + l) * G4; // Factor for 4D unskewing
   double X0 = i - t; // Unskew the cell origin back to (x,y,z,w) space
   double Y0 = j - t;
   double Z0 = k - t;
   double W0 = l - t;

   double x0 = x - X0;  // The x,y,z,w distances from the cell origin
   double y0 = y - Y0;
   double z0 = z - Z0;
   double w0 = w - W0;

   // For the 4D case, the simplex is a 4D shape I won't even try to describe.
   // To find out which of the 24 possible simplices we're in, we need to
   // determine the magnitude ordering of x0, y0, z0 and w0.
   // The method below is a good way of finding the ordering of x,y,z,w and
   // then find the correct traversal order for the simplex we’re in.
   // First, six pair-wise comparisons are performed between each possible pair
   // of the four coordinates, and the results are used to add up binary bits
   // for an integer index.
   int c1 = (x0 > y0) ? 32 : 0;
   int c2 = (x0 > z0) ? 16 : 0;
   int c3 = (y0 > z0) ? 8 : 0;
   int c4 = (x0 > w0) ? 4 : 0;
   int c5 = (y0 > w0) ? 2 : 0;
   int c6 = (z0 > w0) ? 1 : 0;
   int c = c1 + c2 + c3 + c4 + c5 + c6;

   int i1, j1, k1, l1; // The integer offsets for the second simplex corner
   int i2, j2, k2, l2; // The integer offsets for the third simplex corner
   int i3, j3, k3, l3; // The integer offsets for the fourth simplex corner

   // simplex[c] is a 4-vector with the numbers 0, 1, 2 and 3 in some order.
   // Many values of c will never occur, since e.g. x>y>z>w makes x<z, y<w and x<w
   // impossible. Only the 24 indices which have non-zero entries make any sense.
   // We use a thresholding to set the coordinates in turn from the largest magnitude.
   // The number 3 in the "simplex" array is at the position of the largest coordinate.
   i1 = simplex[c][0]>=3 ? 1 : 0;
   j1 = simplex[c][1]>=3 ? 1 : 0;
   k1 = simplex[c][2]>=3 ? 1 : 0;
   l1 = simplex[c][3]>=3 ? 1 : 0;
   // The number 2 in the "simplex" array is at the second largest coordinate.
   i2 = simplex[c][0]>=2 ? 1 : 0;
   j2 = simplex[c][1]>=2 ? 1 : 0;
   k2 = simplex[c][2]>=2 ? 1 : 0;
   l2 = simplex[c][3]>=2 ? 1 : 0;
   // The number 1 in the "simplex" array is at the second smallest coordinate.
   i3 = simplex[c][0]>=1 ? 1 : 0;
   j3 = simplex[c][1]>=1 ? 1 : 0;
   k3 = simplex[c][2]>=1 ? 1 : 0;
   l3 = simplex[c][3]>=1 ? 1 : 0;
   // The fifth corner has all coordinate offsets = 1, so no need to look that up.

   double x1 = x0 - i1 + G4; // Offsets for second corner in (x,y,z,w) coords
   double y1 = y0 - j1 + G4;
   double z1 = z0 - k1 + G4;
   double w1 = w0 - l1 + G4;
   double x2 = x0 - i2 + 2.0f*G4; // Offsets for third corner in (x,y,z,w) coords
   double y2 = y0 - j2 + 2.0f*G4;
   double z2 = z0 - k2 + 2.0f*G4;
   double w2 = w0 - l2 + 2.0f*G4;
   double x3 = x0 - i3 + 3.0f*G4; // Offsets for fourth corner in (x,y,z,w) coords
   double y3 = y0 - j3 + 3.0f*G4;
   double z3 = z0 - k3 + 3.0f*G4;
   double w3 = w0 - l3 + 3.0f*G4;
   double x4 = x0 - 1.0f + 4.0f*G4; // Offsets for last corner in (x,y,z,w) coords
   double y4 = y0 - 1.0f + 4.0f*G4;
   double z4 = z0 - 1.0f + 4.0f*G4;
   double w4 = w0 - 1.0f + 4.0f*G4;

   // Wrap the integer indices at 256, to avoid indexing p[] out of bounds
   int ii = i % 256;
   int jj = j % 256;
   int kk = k % 256;
   int ll = l % 256;

   // Calculate the contribution from the five corners
   double t0 = 0.6f - x0*x0 - y0*y0 - z0*z0 - w0*w0;
   if(t0 < 0.0f) n0 = 0.0f;
   else {
      t0 *= t0;
      n0 = t0 * t0 * grad4(p[ii+p[jj+p[kk+p[ll]]]], x0, y0, z0, w0);
   }

   double t1 = 0.6f - x1*x1 - y1*y1 - z1*z1 - w1*w1;
   if(t1 < 0.0f) n1 = 0.0f;
   else {
      t1 *= t1;
      n1 = t1 * t1 * grad4(p[ii+i1+p[jj+j1+p[kk+k1+p[ll+l1]]]], x1, y1, z1, w1);
   }

   double t2 = 0.6f - x2*x2 - y2*y2 - z2*z2 - w2*w2;
   if(t2 < 0.0f) n2 = 0.0f;
   else {
      t2 *= t2;
      n2 = t2 * t2 * grad4(p[ii+i2+p[jj+j2+p[kk+k2+p[ll+l2]]]], x2, y2, z2, w2);
   }

   double t3 = 0.6f - x3*x3 - y3*y3 - z3*z3 - w3*w3;
   if(t3 < 0.0f) n3 = 0.0f;
   else {
      t3 *= t3;
      n3 = t3 * t3 * grad4(p[ii+i3+p[jj+j3+p[kk+k3+p[ll+l3]]]], x3, y3, z3, w3);
   }

   double t4 = 0.6f - x4*x4 - y4*y4 - z4*z4 - w4*w4;
   if(t4 < 0.0f) n4 = 0.0f;
   else {
      t4 *= t4;
      n4 = t4 * t4 * grad4(p[ii+1+p[jj+1+p[kk+1+p[ll+1]]]], x4, y4, z4, w4);
   }

   // Sum up and scale the result to cover the range [-1,1]
   return 27.0f * (n0 + n1 + n2 + n3 + n4); // TODO: The scale factor is preliminary!
}
//---------------------------------------------------------------------
