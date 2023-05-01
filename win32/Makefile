#
#  Makefile --
#
#     Makefile for building the Wings3D.exe wrapper
#     for starting Wings.
#
#  Copyright (c) 2003-2009 Bjorn Gustavsson & Dan Gudmundsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id$
#

LIBS = -lkernel32 -luser32 -ladvapi32
CFLAGS = -mwindows
OUT = -o
RES_FILE = wings3d_res.o

ifeq ($(findstring CYGWIN,$(shell uname -s)),CYGWIN)
    CC = mingw32-gcc
else
    ifeq ($(WSLcross), true)
	OUT = -nologo -Fe
	CC = cl.exe
	RES_FILE = wings3d.res
	CFLAGS =
	LIBS = /link Shell32.lib user32.lib kernel32.lib advapi32.lib
    else
	CC = gcc
    endif
endif

include ../erl.mk
RELEASE = $(shell $(ERL) -noshell -eval 'erlang:display(erlang:system_info(otp_release))' -s erlang halt)

ifeq ($(RELEASE), "25")
	USE_WERL=" -DUSE_WERL "
endif

Wings3D.exe: wings3d.c $(RES_FILE)
	$(CC) $(USE_WERL)$(OUT)Wings3D.exe wings3d.c $(RES_FILE) $(CFLAGS) $(LIBS)

wings3d_res.o: wings3d.rc
	windres -i wings3d.rc -o wings3d_res.o

wings3d.res: wings3d.rc
	rc.exe wings3d.rc
