/*
 *
 *  wings_file_drv.c --
 *
 *     Erlang driver for native file dialog boxes for Win32.
 *
 *  Copyright (c) 2001 Patrik Nyblom
 *
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *     $Id: wings_file_drv.c,v 1.7 2003/12/26 21:58:08 bjorng Exp $
 */

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef DEBUG
#ifndef __WIN32__
#define ASSERT(X) do {if (!(X)) {erl_exit(1,"%s",#X);} } while(0)
#else
#include <assert.h>
#define ASSERT(X) assert(X)
#endif
#else
#define ASSERT(X)
#endif



#include "erl_driver.h"



/*
** Interface routines
*/
static ErlDrvData wings_file_start(ErlDrvPort port, char *buff);
static void wings_file_stop(ErlDrvData handle);
static int wings_file_control(ErlDrvData handle, unsigned int command, 
			      char* buff, int count, 
			      char** res, int res_size);

/*
** Internal routines
*/

/*
** The driver struct
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
    "wings_file_drv",      /* char *driver_name, the argument to open_port */
    NULL,                  /* F_PTR finish, called when unloaded */
    NULL,                  /* void * that is not used (BC) */
    wings_file_control,    /* F_PTR control, port_control callback */
    NULL,                  /* F_PTR timeout, driver_set_timer callback */
    NULL                   /* F_PTR outputv, reserved */
};

/*
** Driver initialization routine
*/
DRIVER_INIT(wings_file_drv)
{
    return &wings_file_driver_entry;
}

/*
** Driver interface routines
*/

/*
** Open a port
*/
static ErlDrvData wings_file_start(ErlDrvPort port, char *buff)
{
    return (ErlDrvData) 0;
}


/*
** Close a port
*/
static void wings_file_stop(ErlDrvData handle)
{
    return;
}

static void fill_ofn(OPENFILENAME *pofn)
{
    pofn->lStructSize = sizeof(OPENFILENAME);
    pofn->hwndOwner = GetActiveWindow();
    pofn->hInstance = NULL; 
    pofn->lpstrFilter = NULL;     /* For caller to fill in */ 
    pofn->lpstrCustomFilter = NULL; 
    pofn->nMaxCustFilter = 0; 
    pofn->nFilterIndex = 0; 
    pofn->lpstrFile = NULL;       /* For caller to fill in */ 
    pofn->nMaxFile = 0;           /* For caller to fill in */ 
    pofn->lpstrFileTitle = NULL; 
    pofn->nMaxFileTitle = 0; 
    pofn->lpstrInitialDir = NULL; /* For caller to fill in */
    pofn->lpstrTitle = NULL;      /* For caller to fill in */
    pofn->Flags = 0;              /* For caller to fill in */
    pofn->nFileOffset = 0; 
    pofn->nFileExtension = 0; 
    pofn->lpstrDefExt = NULL;     /* For caller to fill in */
    pofn->lCustData = 0; 
    pofn->lpfnHook = NULL; 
    pofn->lpTemplateName = NULL; 
}

/*
** Control message from erlang, syncronous operations which hang the
** emulator. This is not a place where you normally do blocking
** operations, but as the wings application is single threaded
** it doesn't matter.
*/
static int wings_file_control(ErlDrvData handle, unsigned int command, 
			      char* buff, int count, 
			      char** res, int res_size)
{
    OPENFILENAME ofn;
    char *rbuff;
    char *ptr;
    char *defdir;
    char *filter;
    char *filter_desc;
    char *title;
    char *text;
    char *defname;
    UINT style;
    int ret;
    
    switch (command) {
    case 1: /* Open (or import) file */
    case 2: /* Save (or export) file */
        defdir = buff; /* Default directory */
	title = defdir + strlen(defdir) + 1;  /* Title of dialog */
	defname = title + strlen(title) + 1; /* Default name for file */
	filter = defname + strlen(defname) + 1; /* Filter expression (.wings) */
	rbuff=driver_alloc(_MAX_PATH+1);
	strcpy(rbuff, defname);
	fill_ofn(&ofn);
	ofn.lpstrFilter = filter;
        ofn.lpstrFile = rbuff; 
	ofn.nMaxFile = _MAX_PATH+1; 
	ofn.lpstrInitialDir = strlen(defdir) ? defdir : NULL; 
	ofn.lpstrTitle = title; 
	ofn.lpstrDefExt = strlen(filter) ? filter+1 : NULL; 
	if (command == 1) {
	  ofn.Flags = OFN_FILEMUSTEXIST | OFN_HIDEREADONLY; 
	  ret = GetOpenFileName(&ofn);
	} else {
	  ofn.Flags = OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT;
	  ret = GetSaveFileName(&ofn);
	}
	if (ret) {
	    /* Use rbuff instead of default buffer, emulator will free it */
	    *res = rbuff;
	    return strlen(rbuff);
	}
	driver_free(rbuff); /* As it isn't passed to emulator, we have to
			       free it ourselves */
	return 0;
    default:
        return -1; /* Error return, throws exception in erlang */
    }
}
