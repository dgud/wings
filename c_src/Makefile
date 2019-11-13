#
#  Makefile --
#
#     Makefile for building Wings "accelerator" helpers.
#
#  Copyright (c) 2004-2013 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.11 2006/01/14 09:02:38 dgud Exp $
#
include ../erl.mk

OS := $(shell uname -s)

LIBDIR = ../priv

ifeq ($(findstring CYGWIN,$(OS)),CYGWIN)
  USEMINGWGCC := true
  DEF_CC = mingw32-gcc
endif

ifdef MSYSTEM
 ifeq ($(MSYSTEM),$(filter $(MSYSTEM),MINGW32 MINGW64))
  USEMINGWGCC := true
  DEF_CC = gcc
 endif
else
 ifeq ($(WSLcross), true)
  UseMCL = true
 endif
endif

## UseMCL := $(shell cl.exe 2>&1 | grep Microsoft)

ifneq (x$(UseMCL),x)
  UseMCL = true
  USEMINGWGCC = false
  DEF_CC = cl.exe
endif

FRD = 'io:fwrite("~n=:~s~n",[code:root_dir()]),init:stop().'
ERL_DIR := $(shell echo $(FRD) | $(ERL) | sed -n '/^=:/s/^=://p')
ERL_INC = "$(ERL_DIR)/usr/include"

UNIVERSAL_FLAGS =
COMMON_CFLAGS = -Wall

ifeq ($(UseMCL),true)
 OUT = -Fe:
 SO_EXT = dll
 LIBS = -LD
 CFLAGS += -nologo -Zi -Ox
else
 OUT = -o
 ifdef USEMINGWGCC
  SO_EXT = dll
  LIBS = -mdll
  GL_LIBS += -lopengl32 -lglu32
  CFLAGS += $(COMMON_CFLAGS) -Werror -O3
 else
  SO_EXT = so
  DEF_CC = $(CC)
  ifeq ($(OS),Darwin)
    CFLAGS = $(UNIVERSAL_FLAGS) $(COMMON_CFLAGS) -Werror -O3
    LIBS = $(UNIVERSAL_FLAGS) -bundle -flat_namespace -undefined suppress
    GL_LIBS =
  else
    LIBS = -shared -fpic
    GL_LIBS += -L/usr/local/lib -lGLU -lGL
    CFLAGS += $(COMMON_CFLAGS) -Werror -O3
  endif
 endif
endif

ifeq ($(GCC),)
    GCC = $(DEF_CC)
endif

DRV = $(LIBDIR)/wings_pick_nif.$(SO_EXT)

TARGET_FILES=$(BEAM) $(DRV)

all: $(TARGET_FILES)

$(LIBDIR)/%.$(SO_EXT): %.c
	install -d $(LIBDIR)
	$(GCC) $(CFLAGS) -I$(ERL_INC) $(OUT) $@ $< $(LIBS) $(GL_LIBS)

clean:
	rm -f $(TARGET_FILES) vc*.pdb wings_pick_*.obj
	rm -f core
