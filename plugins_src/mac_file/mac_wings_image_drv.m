/*
 *
 *  mac_image_drv.c --
 *
 *     Erlang driver for image reading and writing for Mac
 *
 *  Copyright (c) 2002-2006 Bjorn Gustavsson
 *
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id: mac_wings_image_drv.m,v 1.3 2003/12/31 10:48:19 bjorng Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "erl_driver.h"

#import <Cocoa/Cocoa.h>

#define PATH_MAX 1024

#if (ERL_DRV_EXTENDED_MAJOR_VERSION < 2)
/* R14B or earlier types */
#define ErlDrvSizeT  int
#define ErlDrvSSizeT int
#endif

/*
 * Interface routines
 */
static ErlDrvData mac_image_start(ErlDrvPort port, char *buff);
static void mac_image_stop(ErlDrvData handle);
static ErlDrvSSizeT mac_image_control(ErlDrvData handle, unsigned int command,
				      char* buff, ErlDrvSizeT count,
				      char** res, ErlDrvSizeT res_size);

/*
 * Internal routines
 */

/*
 * The driver struct
 */
ErlDrvEntry mac_image_driver_entry = {
    NULL,		   /* F_PTR init, N/A */
    mac_image_start,      /* L_PTR start, called when port is opened */
    mac_image_stop,       /* F_PTR stop, called when port is closed */
    NULL,                  /* F_PTR output, called when erlang has sent */
    NULL,                  /* F_PTR ready_input, called when input descriptor 
			      ready */
    NULL,                  /* F_PTR ready_output, called when output 
			      descriptor ready */
    "mac_wings_image_drv", /* char *driver_name, the argument to open_port */
    NULL,                  /* F_PTR finish, called when unloaded */
    NULL,                  /* void * that is not used (BC) */
    mac_image_control,    /* F_PTR control, port_control callback */
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
DRIVER_INIT(mac_image_drv)
{
    return &mac_image_driver_entry;
}

/*
 * Driver interface routines
 */

/*
 * Open a port.
 */
static ErlDrvData mac_image_start(ErlDrvPort port, char *buff)
{
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
  return (ErlDrvData) 0;
}

/*
 * Close a port.
 */
static void mac_image_stop(ErlDrvData handle)
{
}

static ErlDrvSSizeT mac_image_control(ErlDrvData handle, unsigned int command,
				      char* buff, ErlDrvSizeT count,
				      char** res, ErlDrvSizeT res_size)
{
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

  switch (command) {
  case 0: {			// Read
    NSString* name = [NSString stringWithCString:buff];
    NSBitmapImageRep* bitmap = [NSBitmapImageRep imageRepWithContentsOfFile:name];

    if (bitmap == nil) {
      *res = NULL;
    } else {
      ErlDrvBinary* bin;
      size_t size;
      char* rbuf;

      size = [bitmap bytesPerPlane];
      bin = driver_alloc_binary(size+16);
      rbuf = bin->orig_bytes;
      ((unsigned *)rbuf)[0] = [bitmap pixelsWide];
      ((unsigned *)rbuf)[1] = [bitmap pixelsHigh];
      ((unsigned *)rbuf)[2] = [bitmap samplesPerPixel];
      ((unsigned *)rbuf)[3] = [bitmap bytesPerRow];
      memcpy(rbuf+16, [bitmap bitmapData], size);
      *res = (void *) bin;
    }
    [pool release];
    return 0;
  }
  case 1: {			// Write
    NSData* nsData;
    NSBitmapImageRep* bitmap;
    NSBitmapImageFileType fileType = 0;

    switch (buff[0]) {
    case 0: fileType = NSPNGFileType; break;
    case 1: fileType = NSGIFFileType; break;
    case 2: fileType = NSJPEGFileType; break;
    }
    buff++, count--;

    nsData = [NSData dataWithBytes:(void *) buff length:(unsigned) count];
    bitmap = [NSBitmapImageRep imageRepWithData:nsData];
    nsData = [bitmap representationUsingType:fileType properties:nil];
    if (nsData == 0) {
      *res = (void *) 0;
    } else {
      ErlDrvBinary* bin = driver_alloc_binary([nsData length]);
      char* rbin = bin->orig_bytes;
      [nsData getBytes:(void *) rbin];
      *res = (void *) bin;
    }

    [pool release];
    return 0;
  }
  default:
    [pool release];
    return -1; /* Error return, throws exception in erlang */
  }
}
