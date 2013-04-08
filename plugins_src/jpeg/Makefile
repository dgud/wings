#
#  Makefile --
#
#     Makefile for building file JPEG image plug-in.
#
#  Copyright (c) 2004-2013 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.7 2006/01/20 14:47:02 giniu Exp $
#

ifeq ($(findstring CYGWIN,$(shell uname -s)),CYGWIN)
  WINDOWS := true
  DEF_CC = mingw32-gcc
endif

ifeq ($(findstring MINGW32,$(shell uname -s)),MINGW32)
  WINDOWS := true
  DEF_CC = gcc
endif

WINGS_INTL=../../intl_tools
LIBDIR = ../../plugins/jpeg

ERL_DIR := $(shell echo 'io:format("~s~n",[code:root_dir()]),init:stop().' | erl | sed -n '/^1>/s/^1> //p')
ERL_INC = $(ERL_DIR)/usr/include
BEAM = $(LIBDIR)/wp8_jpeg_image.beam
LIBS = -shared 

ERLC = erlc
ERLC_FLAGS = -o $(LIBDIR) -I ../../e3d -I ../../src -I $(ESDL_PATH)/include -pa $(ESDL_PATH)
ERL_COMPILE_FLAGS += -Werror $(TYPE_FLAGS) -pa $(WINGS_INTL) -I $(WINGS_INTL) +debug_info

ifdef WINDOWS
  SO_EXT = dll
else
  SO_EXT = so
  DEF_CC = $(CC)
endif

ifeq ($(GCC),)
     GCC = $(DEF_CC)
endif

DRV = $(LIBDIR)/wings_jpeg_image_drv.$(SO_EXT)

opt: $(BEAM) $(DRV)

template: opt
	erl -pa $(WINGS_INTL) -noinput -run tools generate_template_files $(LIBDIR)

lang: template
	cp *.lang $(LIBDIR)
	erl -pa $(WINGS_INTL) -noinput -run tools diff_lang_files $(LIBDIR)

$(LIBDIR)/%.beam: %.erl
	install -d $(LIBDIR)
	$(ERLC) $(ERLC_FLAGS) $(ERL_COMPILE_FLAGS) $<

ifdef WINDOWS

JPEG_DIR = /usr/local

$(LIBDIR)/wings_jpeg_image_drv.dll: wings_jpeg_image_drv.c
	install -d $(LIBDIR)
	$(GCC) -I$(ERL_INC) -o $(LIBDIR)/wings_jpeg_image_drv.dll \
		-I$(JPEG_DIR)/include \
		wings_jpeg_image_drv.c $(LIBS) $(JPEG_DIR)/lib/libjpeg.a

else

$(LIBDIR)/wings_jpeg_image_drv.so: wings_jpeg_image_drv.c
	install -d $(LIBDIR)
	$(GCC) -fPIC -o $(LIBDIR)/wings_jpeg_image_drv.so -I$(ERL_INC) \
	-I/usr/local/include -L/usr/local/lib \
  wings_jpeg_image_drv.c $(LIBS) -ljpeg

endif

