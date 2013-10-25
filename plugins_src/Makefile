#
#  Makefile --
#
#     Makefile for building the plug-ins.
#
#  Copyright (c) 2001-2013 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.20 2006/01/19 22:30:47 giniu Exp $
#

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

ESRC=.
WINGS_INTL=../intl_tools
EBIN=../plugins/default
ERLC=erlc

ifeq ($(TYPE),debug)
TYPE_FLAGS=-DDEBUG
else
TYPE_FLAGS=
endif

MODULES= \
	wpc_test_ask \
	wp9_dialogs

TARGET_FILES= $(MODULES:%=$(EBIN)/%.beam)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -Werror $(TYPE_FLAGS) +debug_info -pa $(WINGS_INTL) -I $(WINGS_INTL)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt debug:
	$(MAKE) TYPE=$@ common

common: $(TARGET_FILES) subdirs

subdirs:
	(cd accel; $(MAKE))
	(cd import_export; $(MAKE))
	(cd primitives; $(MAKE))
	(cd commands; $(MAKE))
	(cd wx_file; $(MAKE))
	(cd autouv; $(MAKE))

template: opt
	erl -pa $(WINGS_INTL) -noinput -run tools generate_template_files $(EBIN)

lang: subdirs_lang template
	cp *.lang $(EBIN)
	erl -pa $(WINGS_INTL) -noinput -run tools diff_lang_files $(EBIN)

subdirs_lang:
	(cd import_export; $(MAKE) lang)
	(cd primitives; $(MAKE) lang)
	(cd commands; $(MAKE) lang)
	(cd autouv; $(MAKE) lang)

clean: subdirs_clean
	rm -f $(TARGET_FILES)
	rm -f core

subdirs_clean:
	(cd accel; $(MAKE) clean)
	(cd import_export; $(MAKE) clean)
	(cd primitives; $(MAKE) clean)
	(cd commands; $(MAKE) clean)
	(cd autouv; $(MAKE) clean)

$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
