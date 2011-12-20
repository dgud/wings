/*
 *
 *  wings_jpg_drv.c --
 *
 *     Erlang driver for reading and writing JPEG files
 *     using libjpeg from IJG (Independent JPEG Group).
 *
 *  Copyright (c) 2004-2010 Bjorn Gustavsson
 *
 *  libjpeg is copyright (C) 1991-1998, Thomas G. Lane.
 *
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id: wings_jpeg_image_drv.c,v 1.3 2004/01/25 13:04:54 bjorng Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include "erl_driver.h"

#include "jpeglib.h"
#include "jerror.h"

#if (ERL_DRV_EXTENDED_MAJOR_VERSION < 2)
/* R14B or earlier types */
#define ErlDrvSizeT  int
#define ErlDrvSSizeT int
#endif

/*
 * Interface routines
 */
static ErlDrvData jpeg_image_start(ErlDrvPort port, char *buff);
static void jpeg_image_stop(ErlDrvData handle);
static ErlDrvSSizeT jpeg_image_control(ErlDrvData handle, unsigned int command,
				       char* buff, ErlDrvSizeT count,
				       char** res, ErlDrvSizeT res_size);

/*
 * Internal functions.
 */
static void jpeg_buffer_src(j_decompress_ptr cinfo, char* buf, ErlDrvSizeT count);
static void jpeg_buffer_dest(j_compress_ptr cinfo, ErlDrvBinary* bin);
static ErlDrvBinary* jpeg_buffer_dest_get_bin(j_compress_ptr cinfo);

/*
 * The driver struct
 */

ErlDrvEntry jpeg_image_driver_entry = {
    NULL,		   /* F_PTR init, N/A */
    jpeg_image_start,      /* L_PTR start, called when port is opened */
    jpeg_image_stop,       /* F_PTR stop, called when port is closed */
    NULL,                  /* F_PTR output, called when erlang has sent */
    NULL,                  /* F_PTR ready_input, called when input descriptor 
			      ready */
    NULL,                  /* F_PTR ready_output, called when output 
			      descriptor ready */
    "wings_jpeg_image_drv", /* char *driver_name, the argument to open_port */
    NULL,                  /* F_PTR finish, called when unloaded */
    NULL,                  /* void * that is not used (BC) */
    jpeg_image_control,    /* F_PTR control, port_control callback */
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
 * Extended error manager info.
 */

struct my_error_mgr {
  struct jpeg_error_mgr pub;	/* "public" fields */

  jmp_buf setjmp_buffer;	/* for return to caller */
};
typedef struct my_error_mgr * my_error_ptr;

/*
 * Driver initialization routine
 */
DRIVER_INIT(jpeg_image_drv)
{
    return &jpeg_image_driver_entry;
}

/*
 * Open a port.
 */
static ErlDrvData
jpeg_image_start(ErlDrvPort port, char *buff)
{
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
  return (ErlDrvData) 0;
}

/*
 * Close a port.
 */
static void
jpeg_image_stop(ErlDrvData handle)
{
}

/*
 * Here's the routine that will replace the standard error_exit method.
 */

METHODDEF(void)
my_error_exit(j_common_ptr cinfo)
{
  /* cinfo->err really points to a my_error_mgr struct, so coerce pointer */
  my_error_ptr myerr = (my_error_ptr) cinfo->err;

#if 0
  /* Always display the message. */
  /* We could postpone this until after returning, if we chose. */
  (*cinfo->err->output_message) (cinfo);
#endif

  /* Return control to the setjmp point */
  longjmp(myerr->setjmp_buffer, 1);
}

static ErlDrvSSizeT
jpeg_image_control(ErlDrvData handle, unsigned int command, 
		   char* buf, ErlDrvSizeT count, 
		   char** res, ErlDrvSizeT res_size)
{
  JSAMPROW row;
  ErlDrvBinary* bin = 0;

  switch (command) {
  case 0: {			/* Read */
    struct jpeg_decompress_struct cinfo;
    ErlDrvSizeT row_stride;		/* physical row width in output buffer */
    int i;
    unsigned char* rbuf;
    struct my_error_mgr jerr;

    cinfo.err = jpeg_std_error(&jerr.pub);
    jerr.pub.error_exit = my_error_exit;

    /* Establish the setjmp return context for my_error_exit to use. */
    if (setjmp(jerr.setjmp_buffer)) {
      /* If we get here, the JPEG code has signaled an error.
       * We need to clean up the JPEG object, close the input file, and return.
       */
      char buffer[JMSG_LENGTH_MAX];

      /* Create the message */
      (cinfo.err->format_message)((j_common_ptr) &cinfo, buffer);
      jpeg_destroy_decompress(&cinfo);

      bin = driver_alloc_binary(4+strlen(buffer));
      rbuf = bin->orig_bytes;
      ((unsigned *)rbuf)[0] = 0;
      rbuf += 4;
      memcpy(rbuf, buffer, strlen(buffer));
      *res = (void *) bin;
      return 0;
    }

    jpeg_create_decompress(&cinfo);
    jpeg_buffer_src(&cinfo, buf, count);

    jpeg_save_markers(&cinfo, JPEG_COM, 0xFFFF);

    for (i = 0; i < 16; i++) {  /* Ignore jpeg application markers */
       jpeg_save_markers(&cinfo, JPEG_APP0 + i, 0xFFFF);
    }

    (void) jpeg_read_header(&cinfo, TRUE);
    (void) jpeg_start_decompress(&cinfo);

    row_stride = cinfo.output_width * cinfo.output_components;
    res_size = row_stride * cinfo.output_height;
    bin = driver_alloc_binary(res_size+12);
    rbuf = bin->orig_bytes;
    ((unsigned *)rbuf)[0] = cinfo.output_width;
    ((unsigned *)rbuf)[1] = cinfo.output_height;
    ((unsigned *)rbuf)[2] = cinfo.output_components;
    rbuf += 12;
    while (cinfo.output_scanline < cinfo.output_height) {
      row = (JSAMPROW) rbuf;
      (void) jpeg_read_scanlines(&cinfo, &row, 1);
      rbuf += row_stride;
    }
    (void) jpeg_finish_decompress(&cinfo);
    *res = (void *) bin;
    return 0;
  }
  case 1: {			/* Write */
    struct jpeg_compress_struct cinfo;
    struct jpeg_error_mgr jerr;
    int row_stride;		/* physical row width */

    bin = driver_alloc_binary(count);

    cinfo.err = jpeg_std_error(&jerr);
    jpeg_create_compress(&cinfo);

    jpeg_buffer_dest(&cinfo, bin);
    cinfo.image_width = ((unsigned *)buf)[0];
    cinfo.image_height = ((unsigned *)buf)[1];
    cinfo.input_components = ((unsigned *)buf)[2];
    cinfo.in_color_space = JCS_RGB;
    jpeg_set_defaults(&cinfo);
    buf += 12;

    jpeg_start_compress(&cinfo, TRUE);
    row_stride = cinfo.input_components * cinfo.image_width;
        
    while (cinfo.next_scanline < cinfo.image_height) {
      row = (JSAMPROW) buf;
      (void) jpeg_write_scanlines(&cinfo, &row, 1);
      buf += row_stride;
    }

    jpeg_finish_compress(&cinfo);
    bin = jpeg_buffer_dest_get_bin(&cinfo);
    jpeg_destroy_compress(&cinfo);
    *res = (void *) bin;
    return 0;
  }
  default:
    return -1;			/* Error return, throws exception in erlang */
  }
}

/*
 * Being slightly paranoid :), I don't want to use the
 * stdio-based data sources and destinations, as stdio is
 * not used anywhere else in the Erlang virtual machine.
 *
 * Here is a memory based source.
 */

/* Expanded data source object for memory */

typedef struct {
  struct jpeg_source_mgr pub;	/* public fields */
  JOCTET* current;		/* current pointer into buffer */
  size_t left;			/* number of bytes left in buffer */
} MemSourceMgr;

/*
 * Initialize source --- called by jpeg_read_header
 * before any data is actually read.
 */

METHODDEF(void)
init_source (j_decompress_ptr cinfo)
{
  /* Nothing to do here. */
}

METHODDEF(boolean)
fill_input_buffer(j_decompress_ptr cinfo)
{
  MemSourceMgr* src = (MemSourceMgr *) cinfo->src;

  if (src->left == 0) {
    ERREXIT(cinfo, JERR_INPUT_EMPTY);
  }

  src->pub.next_input_byte = src->current;
  src->pub.bytes_in_buffer = src->left;
  src->left = 0;
  return TRUE;
}


METHODDEF(void)
skip_input_data (j_decompress_ptr cinfo, long num_bytes)
{
   /* We assume that this function will never get called. */
   /* fprintf(stderr, "skip_input_data\r\n"); */
}

METHODDEF(void)
term_source (j_decompress_ptr cinfo)
{
  /* no work necessary here */
}

/*
 * Prepare for input from a memory buffer.
 */

static void
jpeg_buffer_src(j_decompress_ptr cinfo, char* buf, ErlDrvSizeT count)
{
  MemSourceMgr* src;

  if (cinfo->src == NULL) {	/* first time for this JPEG object? */
    cinfo->src = (struct jpeg_source_mgr *)
      (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_PERMANENT,
				  sizeof(MemSourceMgr));
    src = (MemSourceMgr *) cinfo->src;
  }

  src = (MemSourceMgr *) cinfo->src;
  src->pub.init_source = init_source;
  src->pub.fill_input_buffer = fill_input_buffer;
  src->pub.skip_input_data = skip_input_data;
  src->pub.resync_to_restart = jpeg_resync_to_restart; /* use default method */
  src->pub.term_source = term_source;

  src->current = (JOCTET *) buf;
  src->left = (size_t) count;
  src->pub.bytes_in_buffer = 0; /* forces fill_input_buffer on first read */
  src->pub.next_input_byte = NULL; /* until buffer loaded */
}

/*
 * And here is a memory destination.
 */


typedef struct {
  struct jpeg_destination_mgr pub; /* public fields */

  ErlDrvBinary* bin;		/* binary that holds output */
  JOCTET* buffer;		/* start of buffer */
} my_destination_mgr;

typedef my_destination_mgr * my_dest_ptr;

#define OUTPUT_BUF_SIZE  4096	/* choose an efficiently fwrite'able size */


/*
 * Initialize destination --- called by jpeg_start_compress
 * before any data is actually written.
 */

METHODDEF(void)
init_destination (j_compress_ptr cinfo)
{
  my_dest_ptr dest = (my_dest_ptr) cinfo->dest;

  /* Allocate the output buffer --- it will be released when done with image */
  dest->buffer = (JOCTET *)
      (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_IMAGE,
				  OUTPUT_BUF_SIZE * sizeof(JOCTET));

  dest->pub.next_output_byte = dest->bin->orig_bytes;
  dest->pub.free_in_buffer = dest->bin->orig_size;
}


/*
 * Empty the output buffer --- called whenever buffer fills up.
 */

METHODDEF(boolean)
empty_output_buffer (j_compress_ptr cinfo)
{
  my_dest_ptr dest = (my_dest_ptr) cinfo->dest;
  unsigned size_used = dest->bin->orig_size - dest->pub.free_in_buffer;

  dest->bin = driver_realloc_binary(dest->bin, 2*dest->bin->orig_size);

  dest->pub.next_output_byte = dest->bin->orig_bytes + size_used;
  dest->pub.free_in_buffer = dest->bin->orig_size - size_used;

  return TRUE;
}


/*
 * Terminate destination --- called by jpeg_finish_compress
 * after all data has been written.
 */

METHODDEF(void)
term_destination (j_compress_ptr cinfo)
{
  my_dest_ptr dest = (my_dest_ptr) cinfo->dest;
  size_t datacount = dest->bin->orig_size - dest->pub.free_in_buffer;

  dest->bin = driver_realloc_binary(dest->bin, datacount);
}

static void
jpeg_buffer_dest(j_compress_ptr cinfo, ErlDrvBinary* bin)
{
  my_dest_ptr dest;

  if (cinfo->dest == NULL) {	/* first time for this JPEG object? */
    cinfo->dest = (struct jpeg_destination_mgr *)
      (*cinfo->mem->alloc_small) ((j_common_ptr) cinfo, JPOOL_PERMANENT,
				  sizeof(my_destination_mgr));
  }

  dest = (my_dest_ptr) cinfo->dest;
  dest->pub.init_destination = init_destination;
  dest->pub.empty_output_buffer = empty_output_buffer;
  dest->pub.term_destination = term_destination;
  dest->bin = bin;
}

static ErlDrvBinary*
jpeg_buffer_dest_get_bin(j_compress_ptr cinfo)
{
  my_dest_ptr dest = (my_dest_ptr) cinfo->dest;
  return dest->bin;
}
