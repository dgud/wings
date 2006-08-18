#
#  Makefile --
#
#     Makefile for building file dialog plug-in for Windows.
#
#  Copyright (c) 2003-2004 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.15 2006/01/19 22:30:47 giniu Exp $
#

WINGS_INTL=../../intl_tools
LIBDIR = ../../plugins/win32_file

ERL_DIR := $(shell echo 'io:format("~s~n",[code:root_dir()]),init:stop().' | erl | sed -n '/^1>/s/^1> //p')
ERL_INC = $(ERL_DIR)/usr/include
BEAM = $(LIBDIR)/wp8_file.beam
LIBS = -shared -lcomdlg32

ERLC = erlc
ERLC_FLAGS = -o $(LIBDIR) -I ../../e3d -I ../../src -I $(ESDL_PATH)/include -pa $(ESDL_PATH) -W +warn_unused_vars +debug_info -pa $(WINGS_INTL) -I $(WINGS_INTL)

DRV = $(LIBDIR)/wings_file_drv.dll

opt: $(BEAM) $(DRV)

template: opt
	erl -pa $(WINGS_INTL) -noinput -run tools generate_template_file $(LIBDIR) wp8_file -run erlang halt

lang: template
	@cp *.lang $(LIBDIR)
	@for LANG_FILE in $(LIBDIR)/wp8_file_*.lang; do \
		case $$LANG_FILE  in \
			$(LIBDIR)/wp8_file_en.lang) ;; \
			*) echo "Processing: $$LANG_FILE"; \
	erl -pa $(WINGS_INTL) -noinput -eval "tools:diff(\"$$LANG_FILE\")" -run erlang halt >> $$LANG_FILE ;; \
		esac \
	done

$(LIBDIR)/%.beam: %.erl
	install -d $(LIBDIR)
	$(ERLC) $(ERLC_FLAGS) $<

$(LIBDIR)/wings_file_drv.dll: wings_file_drv.c
	install -d $(LIBDIR)
	mingw32-gcc -I$(ERL_INC) -o $(LIBDIR)/wings_file_drv.dll wings_file_drv.c $(LIBS)
