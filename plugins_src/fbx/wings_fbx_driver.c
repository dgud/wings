/*
 *  wings_fbx.c --
 *
 *     Erlang driver for FBX file import/export.
 *
 *  Copyright (c) 2003-2005 Bjorn Gustavsson
 *
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id: wings_fbx_driver.c,v 1.1 2005/03/11 05:55:48 bjorng Exp $
 */

#include "erl_driver.h"

/*
 * Interface routines.
 */
static ErlDrvData wings_file_start(ErlDrvPort port, char *buff);
static void wings_file_stop(ErlDrvData handle);
static int control(ErlDrvData handle, unsigned int command, 
                   char* buff, int count, 
                   char** res, int res_size);

int fbx_control(unsigned int command, 
                char* buff, int count, 
                char** res, int res_size);

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
    WINGS_FBX_DRIVER_NAME, /* char *driver_name, the argument to open_port */
    NULL,                  /* F_PTR finish, called when unloaded */
    NULL,                  /* void * that is not used (BC) */
    control,               /* F_PTR control, port_control callback */
    NULL,                  /* F_PTR timeout, driver_set_timer callback */
    NULL                   /* F_PTR outputv, reserved */
};

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
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
  return (ErlDrvData) 0;
}

/*
 * Close a port
 */
static void wings_file_stop(ErlDrvData handle)
{
  return;
}

static int
control(ErlDrvData handle, unsigned int command,
        char* buff, int count, 
        char** res, int res_size)
{
  *res = 0;
  return fbx_control(command, buff, count, res, res_size);
}

void
send_response(char** res, char* s, int len)
{
   ErlDrvBinary* bin;

   bin = driver_alloc_binary(len); 
   memcpy(bin->orig_bytes, s, len);
   *res = (char *) bin;
}

