#
#  Makefile --
#
#     Makefile for building Wings "accelerator" helpers.
#
#  Copyright (c) 2004-2005 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.11 2006/01/14 09:02:38 dgud Exp $
#
OS := $(shell uname -s)

ifeq ($(findstring CYGWIN,$(OS)),CYGWIN)
  CYGWIN := true
endif

LIBDIR = ../../plugins/accel

ERL_DIR := $(shell echo 'io:format("~s~n",[code:root_dir()]),init:stop().' | erl | sed -n '/^1>/s/^1> //p')
ERL_INC = $(ERL_DIR)/usr/include
BEAM = $(LIBDIR)/wpc_ogla.beam \
	$(LIBDIR)/wpc_pnoise.beam \
	$(LIBDIR)/pnoise.beam

ifdef CYGWIN
 SO_EXT = dll
 LIBS = -shared
 GL_LIBS = -lopengl32 -lglu32
 GCC = mingw32-gcc
else
 SO_EXT = so
 GCC = gcc
 ifeq ($(OS),Darwin)
   LIBS = -bundle -flat_namespace -undefined suppress
   GL_LIBS =
 else
   LIBS = -shared -fpic
   GL_LIBS = -lGLU -lGL
 endif
endif

ERLC = erlc
ERLC_FLAGS = -o $(LIBDIR) +debug_info

DRV = $(LIBDIR)/wings_ogla_drv.$(SO_EXT) \
	$(LIBDIR)/perlin_noise_drv.$(SO_EXT)

TARGET_FILES=$(BEAM) $(DRV)

all: $(TARGET_FILES)

$(LIBDIR)/%.beam: %.erl
	install -d $(LIBDIR)
	$(ERLC) $(ERLC_FLAGS) $<

$(LIBDIR)/%.$(SO_EXT): %.c
	install -d $(LIBDIR)
	$(GCC) -I$(ERL_INC) -o $@ $< $(LIBS) $(GL_LIBS)

clean:
	rm -f $(TARGET_FILES)
	rm -f core
