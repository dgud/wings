/*
 *  mac_wings_file_drv.c --
 *
 *     Erlang driver for native file dialog boxes for Mac OS X.
 *
 *  Copyright (c) 2001-2010 Patrik Nyblom, Bjorn Gustavsson.
 *
 *  Modified to support OSX by Sean Hinde
 *
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id: mac_wings_file_drv.c,v 1.12 2003/06/19 08:21:28 bjorng Exp $
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
** Interface routines
*/
static ErlDrvData mac_wings_file_start(ErlDrvPort port, char *buff);
static void mac_wings_file_stop(ErlDrvData handle);
static ErlDrvSSizeT mac_wings_file_control(ErlDrvData handle, unsigned int command,
					   char* buff, ErlDrvSizeT count,
					   char** res, ErlDrvSizeT res_size);

/*
** Internal routines
*/

/*
** The driver struct
*/
ErlDrvEntry mac_wings_file_driver_entry = {
    NULL,		   /* F_PTR init, N/A */
    mac_wings_file_start,      /* L_PTR start, called when port is opened */
    mac_wings_file_stop,       /* F_PTR stop, called when port is closed */
    NULL,                  /* F_PTR output, called when erlang has sent */
    NULL,                  /* F_PTR ready_input, called when input descriptor 
			      ready */
    NULL,                  /* F_PTR ready_output, called when output 
			      descriptor ready */
    "mac_wings_file_drv",      /* char *driver_name, the argument to open_port */
    NULL,                  /* F_PTR finish, called when unloaded */
    NULL,                  /* void * that is not used (BC) */
    mac_wings_file_control,    /* F_PTR control, port_control callback */
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
** Driver initialization routine
*/
DRIVER_INIT(mac_wings_file_drv)
{
    return &mac_wings_file_driver_entry;
}

/*
** Driver interface routines
*/

/*
** Open a port
*/
static ErlDrvData mac_wings_file_start(ErlDrvPort port, char *buff)
{
    return (ErlDrvData) 0;
}


/*
** Close a port
*/
static void mac_wings_file_stop(ErlDrvData handle)
{
    return;
}

/*
** Control message from erlang, syncronous operations which hang the
** emulator. This is not a place where you normally do blocking
** operations, but as the wings application is single threaded
** it doesn't matter.
*/
static ErlDrvSSizeT mac_wings_file_control(ErlDrvData handle, unsigned int command,
					   char* buff, ErlDrvSizeT count,
					   char** res, ErlDrvSizeT res_size)
{
  int result;
  char *rbuff = 0;
  char *defdir;
  char *filter;
  char *title;
  char *text;
  char *defname;
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

  switch (command) {
    case 1: /* Open (or import) file */
    case 2: /* Save (or export) file */
      {
	NSString* defdir1;
	NSString* title1;
	NSString* defname1;
          
	defdir = buff; /* Default directory */
	title = defdir + strlen(defdir) + 1;  /* Title of dialog */
	defname = title + strlen(title) + 1; /* Default name for file */
	filter = defname + strlen(defname) + 1; /* Filter expression (.wings) */

        defdir1 = [NSString stringWithUTF8String:defdir];
        title1 = [NSString stringWithUTF8String:title];
        defname1 = [NSString stringWithUTF8String:defname];
	
	if (command == 1) {	/* Open/Import */
	  NSOpenPanel* oPanel = [NSOpenPanel openPanel];
	  NSMutableArray* fileTypes = [NSMutableArray arrayWithCapacity:10];

	  while (filter[0] != 0) {
	    NSString* AFilter = [NSString stringWithUTF8String:filter];

	    [fileTypes addObject:AFilter];
	    filter += strlen(filter) + 1;
	  }
	  [oPanel setAllowsMultipleSelection:NO];
	  [oPanel setTitle:title1];
	  result = [oPanel runModalForDirectory:defdir1 file:nil types:fileTypes];
	  if (result == NSOKButton) {
	    NSString *aFile = [oPanel filename];
	    const char* utf8str = [aFile UTF8String];
	    rbuff = driver_alloc(strlen(utf8str)+1);
	    strcpy(rbuff, utf8str);
	    *res = rbuff;
	    [pool release];
	    return strlen(rbuff);
	  }
	} else {		/* Save/Export */
	  NSSavePanel *sPanel = [NSSavePanel savePanel];
	  NSString* fileType = [NSString stringWithUTF8String:filter];

	  [sPanel setRequiredFileType:fileType];
	  [sPanel setTitle:title1];
	  result = [sPanel runModalForDirectory:defdir1 file:defname1];
	  if (result == NSOKButton) {
	    NSString *aFile = [sPanel filename];
	    const char* utf8str = [aFile UTF8String];
	    rbuff = driver_alloc(strlen(utf8str)+1);
	    strcpy(rbuff, utf8str);
	    *res = rbuff;
	    [pool release];
	    return strlen(rbuff);
	  }
	}
	if (rbuff) {
	  driver_free(rbuff);
	}
	[pool release];
	return 0;
      }
    default:
      [pool release];
      return -1; /* Error return, throws exception in erlang */
  }
}
