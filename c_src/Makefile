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

WORDSIZE := $(shell $(ERL) -noinput -eval 'io:format("~w~n", [erlang:system_info(wordsize)*8]),init:stop()')

UNIVERSAL_FLAGS =
COMMON_CFLAGS = -g

ifeq ($(UseMCL),true)
 OUT = -Fe:
 OUT_OBJ = -Fo
 OBJ = obj
 SO_EXT = dll
 LIBS = -LD
 CFLAGS += -nologo -Zi -Ox /FS /MP -D__WIN32__
 CXXFLAGS += $(CFLAGS) /EHsc
else
 OUT = -o
 OUT_OBJ = -o
 OBJ = o
 CFLAGS += $(COMMON_CFLAGS) -O3
 CXXFLAGS += -Wno-deprecated-declarations -Wno-unknown-pragmas -Wno-switch

 ifdef USEMINGWGCC
  SO_EXT = dll
  LIBS = -mdll
 else
  SO_EXT = so
  DEF_CC = $(CC)
  ifeq ($(OS),Darwin)
    CFLAGS = $(UNIVERSAL_FLAGS) $(COMMON_CFLAGS) -O3
    LIBS = $(UNIVERSAL_FLAGS) -bundle -flat_namespace -undefined suppress
    CXXFLAGS += $(CFLAGS) -std=c++14
  else
    LIBS = -shared -fpic
    CXX_LD = -lstdc++
    CXXFLAGS += $(CFLAGS)
  endif
 endif
endif

ifeq ($(GCC),)
    GCC = $(DEF_CC)
endif

IGL_INCLUDE = -I../_deps/eigen -I../_deps/libigl/include

# libigl only supports 64bits systems
ifeq (x"${WORDSIZE}",x"64")
    DRV = $(LIBDIR)/libigl.$(SO_EXT) $(LIBDIR)/wings_pick_nif.$(SO_EXT)
else
    $(info 32bits build ignoring libigl not supported)
    DRV = $(LIBDIR)/wings_pick_nif.$(SO_EXT)
endif

TARGET_FILES=$(DRV)

opt debug: $(TARGET_FILES)

$(LIBDIR)/libigl.$(SO_EXT): libigl.cpp
	install -d $(LIBDIR)
	$(GCC) $(CXXFLAGS) $(IGL_OPTS) -I$(ERL_INC) $(IGL_INCLUDE) $(OUT)$@ $< $(LIBS) $(CXX_LD)

$(LIBDIR)/%.$(SO_EXT): %.c
	install -d $(LIBDIR)
	$(GCC) $(CFLAGS) -I$(ERL_INC) $(OUT) $@ $< $(LIBS)

clean:
	rm -f $(TARGET_FILES) vc*.pdb *.$(OBJ)
	rm -f core
