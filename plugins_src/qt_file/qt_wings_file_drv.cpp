/*
 *
 *  qt_wings_file_drv.c --
 *
 *     Erlang driver for native file dialog boxes for QT.
 *
 *  Original wings_file_drv.c Copyright (c) 2001 Patrik Nyblom
 *
 *  Changes for QT support by Chris Osgood : 2001/12/14
 *
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */
#include <qapplication.h>
#include <qmessagebox.h>
#include <qfiledialog.h>

extern "C"
{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "erl_driver.h"

#ifndef PATH_MAX
# define PATH_MAX 1024
#endif

/*
** Interface routines
*/
static ErlDrvData qt_wings_file_start(ErlDrvPort port, char *buff);
static void qt_wings_file_stop(ErlDrvData handle);
static int qt_wings_file_control(ErlDrvData handle, unsigned int command,
                              char* buff, int count,
                              char** res, int res_size);

/*
** Internal routines
*/

/*
** The driver struct
*/
ErlDrvEntry qt_wings_file_driver_entry =
    {
        NULL,		   /* F_PTR init, N/A */
        qt_wings_file_start,   /* L_PTR start, called when port is opened */
        qt_wings_file_stop,    /* F_PTR stop, called when port is closed */
        NULL,                  /* F_PTR output, called when erlang has sent */
        NULL,                  /* F_PTR ready_input, called when input descriptor ready */
        NULL,                  /* F_PTR ready_output, called when output descriptor ready */
        "qt_wings_file_drv",   /* char *driver_name, the argument to open_port */
        NULL,                  /* F_PTR finish, called when unloaded */
        NULL,                  /* void * that is not used (BC) */
        qt_wings_file_control, /* F_PTR control, port_control callback */
        NULL,                  /* F_PTR timeout, driver_set_timer callback */
        NULL                   /* F_PTR outputv, reserved */
    };

/*
** Driver initialization routine
*/
DRIVER_INIT(qt_wings_file_drv)
{
    return &qt_wings_file_driver_entry;
}

/*
** Driver interface routines
*/

/*
** Open a port
*/
static ErlDrvData qt_wings_file_start(ErlDrvPort port, char *buff)
{
    return (ErlDrvData) 0;
}


/*
** Close a port
*/
static void qt_wings_file_stop(ErlDrvData handle)
{
    return;
}

/*
** Control message from erlang, syncronous operations which hang the
** emulator. This is not a place where you normally do blocking
** operations, but as the wings application is single threaded
** it doesn't matter.
*/
static int qt_wings_file_control(ErlDrvData handle, unsigned int command,
                              char* buff, int count,
                              char** res, int res_size)
{
    char *rbuff;
    char *defdir;
    char *filter;
    char *filter_desc;
    char *title;
    char *text;
    char *defname;

    /* Start a new QT application (with no arguments) */
    int arg1 = 0;
    char *arg2[] = { "null" };
    QApplication a(arg1, arg2);

    switch (command)
    {
    case 0: /* Yes/No/Cancel question */
        {
            title = buff; /* Title of window */
            text = title + strlen(title) + 1; /* Prompt text */

            QMessageBox mb(title, text, QMessageBox::Warning,
			   QMessageBox::Yes | QMessageBox::Default,
			   QMessageBox::No,
			   QMessageBox::Cancel | QMessageBox::Escape);

            switch(mb.exec()) {
            case (QMessageBox::Yes): // YES
                strcpy(*res, "yes");
                return 3;
            case (QMessageBox::No): // NO
                strcpy(*res, "no");
                return 2;
            default:
                strcpy(*res,"aborted");
                return 7;
            }
        }
    case 1: /* Open (or import) file */
    case 2: /* Save (or export) file */
        {
	    defdir = buff; /* Default directory */
	    title = defdir + strlen(defdir) + 1;  /* Title of dialog */
	    defname = title + strlen(title) + 1; /* Default name for file */
	    filter = defname + strlen(defname) + 1; /* Filter expression (.wings) */

            /* Create our file dialog */
            QFileDialog fd(defdir, 0, 0, title, TRUE);

            /* Build list of filters */
            QStringList qFilterlist;


	    while (filter[0] != 0) {
		QString qFilterWings = filter;

		qFilterlist.append(qFilterWings);
		filter += strlen(filter) + 1;
	    }

            /* Pre-dialog setup */
            fd.setFilters(qFilterlist);
            fd.setSelection(defname);
            fd.setCaption(title);

	    if (command == 1) {
		fd.setMode(QFileDialog::ExistingFile);
	    }

            /* Start the dialog and wait for a result */
            if (fd.exec()) {
                QString ret = fd.selectedFile();

                if (!ret.isEmpty()) {
            	    rbuff = (char *)driver_alloc(PATH_MAX+1);
                    strncpy(rbuff, ret, PATH_MAX+1);
                    *res = rbuff;
                    return strlen(rbuff);
                }
            }

	    return 0;
        }
    case 3: /* Message box */
    {
            title = buff; /* Title of window */
            text = title + strlen(title) + 1; /* Prompt text */
            QMessageBox::information(0, title, text, QMessageBox::Ok | QMessageBox::Default);
            return 0;
        }
    default:
        return -1; /* Error return, throws exception in erlang */
    }
}

/* end extern "C" */
}
