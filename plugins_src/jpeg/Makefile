#
#  Makefile --
#
#     Makefile for building file JPEG image plug-in.
#
#  Copyright (c) 2004 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.7 2006/01/20 14:47:02 giniu Exp $
#

ifeq ($(findstring CYGWIN,$(shell uname -s)),CYGWIN)
  CYGWIN := true
endif

WINGS_INTL=../../intl_tools
LIBDIR = ../../plugins/jpeg

ERL_DIR := $(shell echo 'io:format("~s~n",[code:root_dir()]),init:stop().' | erl | sed -n '/^1>/s/^1> //p')
ERL_INC = $(ERL_DIR)/usr/include
BEAM = $(LIBDIR)/wp8_jpeg_image.beam
LIBS = -shared -ljpeg

ERLC = erlc
ERLC_FLAGS = -o $(LIBDIR) -I ../../e3d -I ../../src -I $(ESDL_PATH)/include -pa $(ESDL_PATH)
ERL_COMPILE_FLAGS += -W $(TYPE_FLAGS) +warn_unused_vars +debug_info -pa $(WINGS_INTL) -I $(WINGS_INTL)

ifdef CYGWIN
SO_EXT = dll
else
SO_EXT = so
endif

DRV = $(LIBDIR)/wings_jpeg_image_drv.$(SO_EXT)

opt: $(BEAM) $(DRV)

template: opt
	erl -pa $(WINGS_INTL) -noinput -run tools generate_template_file $(LIBDIR) wp8_jpeg_image -run erlang halt

lang: template
	@cp *.lang $(LIBDIR)
	@for LANG_FILE in $(LIBDIR)/wp8_jpeg_image_*.lang; do \
		case $$LANG_FILE  in \
			$(LIBDIR)/wp8_jpeg_image_en.lang) ;; \
			*) echo "Processing: $$LANG_FILE"; \
	erl -pa $(WINGS_INTL) -noinput -eval "tools:diff(\"$$LANG_FILE\")" -run erlang halt >> $$LANG_FILE ;; \
		esac \
	done

$(LIBDIR)/%.beam: %.erl
	install -d $(LIBDIR)
	$(ERLC) $(ERLC_FLAGS) $(ERL_COMPILE_FLAGS) $<

ifdef CYGWIN

$(LIBDIR)/wings_jpeg_image_drv.dll: wings_jpeg_image_drv.c
	install -d $(LIBDIR)
	mingw32-gcc -I$(ERL_INC) -o $(LIBDIR)/wings_jpeg_image_drv.dll \
  wings_jpeg_image_drv.c $(LIBS)

else

$(LIBDIR)/wings_jpeg_image_drv.so: wings_jpeg_image_drv.c
	install -d $(LIBDIR)
	gcc -o $(LIBDIR)/wings_jpeg_image_drv.so -I$(ERL_INC) \
  wings_jpeg_image_drv.c $(LIBS)

endif

