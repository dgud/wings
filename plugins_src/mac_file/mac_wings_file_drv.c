/*
 *
 *  mac_wings_file_drv.c --
 *
 *     Erlang driver for native file dialog boxes for Mac OS X.
 *
 *  Copyright (c) 2001-2003 Patrik Nyblom, Bjorn Gustavsson.
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

/*
** Interface routines
*/
static ErlDrvData mac_wings_file_start(ErlDrvPort port, char *buff);
static void mac_wings_file_stop(ErlDrvData handle);
static int mac_wings_file_control(ErlDrvData handle, unsigned int command, 
			      char* buff, int count, 
			      char** res, int res_size);

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
    NULL                   /* F_PTR outputv, reserved */
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
static int mac_wings_file_control(ErlDrvData handle, unsigned int command, 
			      char* buff, int count, 
			      char** res, int res_size)
{
  int result;
  char *rbuff;
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

        defdir1 = [NSString stringWithCString:defdir];
        title1 = [NSString stringWithCString:title];
        defname1 = [NSString stringWithCString:defname];
	
	rbuff = driver_alloc(PATH_MAX+1);
	strcpy(rbuff, defname);
	
	if (command == 1) {
	  NSOpenPanel* oPanel = [NSOpenPanel openPanel];
	  NSMutableArray* fileTypes = [NSMutableArray arrayWithCapacity:10];

	  while (filter[0] != 0) {
	    NSString* AFilter = [NSString stringWithCString:filter];

	    [fileTypes addObject:AFilter];
	    filter += strlen(filter) + 1;
	  }
	  [oPanel setAllowsMultipleSelection:NO];
	  [oPanel setTitle:title1];
	  result = [oPanel runModalForDirectory:defdir1 file:nil types:fileTypes];
	  if (result == NSOKButton) {
	    NSString *aFile = [oPanel filename];
	    [aFile getCString:rbuff];
	    *res = rbuff;
	    [pool release];
	    return strlen(rbuff);
	  }
	} else {
	  NSSavePanel *sPanel = [NSSavePanel savePanel];
	  NSString* fileType = [NSString stringWithCString:filter];

	  [sPanel setRequiredFileType:fileType];
	  [sPanel setTitle:title1];
	  result = [sPanel runModalForDirectory:defdir1 file:defname1];
	  if (result == NSOKButton) {
	    NSString *aFile = [sPanel filename];
	    [aFile getCString:rbuff];
	    *res = rbuff;
	    [pool release];
	    return strlen(rbuff);
	  }
	}
	driver_free(rbuff);
	[pool release];
	return 0;
      }
    default:
      [pool release];
      return -1; /* Error return, throws exception in erlang */
  }
}
