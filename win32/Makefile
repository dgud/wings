#
#  Makefile --
#
#     Makefile for building the Wings3D.exe wrapper
#     for starting Wings.
#
#  Copyright (c) 2003-2009 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id$
#

LIBS = -lkernel32 -luser32 -ladvapi32

ifeq ($(findstring CYGWIN,$(shell uname -s)),CYGWIN)
  GCC = mingw32-gcc
else
  GCC = gcc
endif


Wings3D.exe: wings3d.c wings3d_res.o
	$(GCC) -o Wings3D.exe wings3d.c wings3d_res.o -mwindows $(LIBS)

wings3d_res.o: wings3d.rc
	windres -i wings3d.rc -o wings3d_res.o
