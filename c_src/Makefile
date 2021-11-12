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
 DEBUG = -Fd$@.pdb
 CFLAGS += -nologo -Zi -Ox -FS -MP -D__WIN32__
 CXXFLAGS += $(CFLAGS) -EHsc
else
 OUT = -o
 OUT_OBJ = -o
 OBJ = o
 CFLAGS += $(COMMON_CFLAGS) -O3 -fPIC
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
    LIBS = -shared
    CXX_LD = -lstdc++
    CXXFLAGS += $(CFLAGS)
  endif
 endif
endif

ifeq ($(GCC),)
    GCC = $(DEF_CC)
endif

IGL_INCLUDE = -I../_deps/eigen -I../_deps/libigl/include

TESS_SRC = dict geom memalloc mesh normal priorityq render sweep tess tessmono
TESS_HEADER = $(TESS_SRC) priorityq-sort dict-list priorityq-heap

# TESS_SRC_FILES = $(TESS_SRC:%=tess/%.c)
TESS_OBJ_FILES = $(TESS_SRC:%=tess/%.$(OBJ))
TESS_HEADER_FILES = $(TESS_HEADER:%=tess/%.h) tess/wtess.h

# libigl only supports 64bits systems
ifeq (x"${WORDSIZE}",x"64")
    DRV = libigl wings_pick_nif wings_tess
else
    $(info 32bits build ignoring libigl not supported)
    DRV = wings_pick_nif wings_tess
endif

TARGET_FILES=$(DRV:%=$(LIBDIR)/%.$(SO_EXT))

opt debug: $(TARGET_FILES)

$(LIBDIR)/libigl.$(SO_EXT): libigl.cpp
	install -d $(LIBDIR)
	$(GCC) $(CXXFLAGS) $(IGL_OPTS) -I$(ERL_INC) $(IGL_INCLUDE) $(DEBUG) $(OUT)$@ $< $(LIBS) $(CXX_LD)

$(LIBDIR)/wings_tess.$(SO_EXT): $(TESS_OBJ_FILES) wings_tess.c
	install -d $(LIBDIR)
	$(GCC) $(CFLAGS) -I$(ERL_INC) $(DEBUG) $(OUT)$@ wings_tess.c $(TESS_OBJ_FILES) $(LIBS)

$(LIBDIR)/%.$(SO_EXT): %.c
	install -d $(LIBDIR)
	$(GCC) $(CFLAGS) -I$(ERL_INC) $(DEBUG) $(OUT) $@ $< $(LIBS)

tess/%.$(OBJ): tess/%.c $(TESS_HEADER_FILES)
	$(GCC) $(CFLAGS) -c $(OUT_OBJ)$@ $<

clean:
	rm -f $(TARGET_FILES) vc*.pdb *.$(OBJ)
	rm -f tess/*.$(OBJ)
	rm -f core
