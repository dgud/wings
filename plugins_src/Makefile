#
#  Makefile --
#
#     Makefile for building the plug-ins.
#
#  Copyright (c) 2001-2005 Bjorn Gustavsson
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

TRANSLATABLE= \
	wp9_dialogs \

TARGET_FILES= $(MODULES:%=$(EBIN)/%.beam)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += 
ERL_COMPILE_FLAGS += -W $(TYPE_FLAGS) +warn_unused_vars +debug_info -pa $(WINGS_INTL) -I $(WINGS_INTL)

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
	(cd autouv; $(MAKE))

template: opt
	@for FILE in $(TRANSLATABLE); do \
		erl -pa $(WINGS_INTL) -noinput -run tools generate_template_file $(EBIN) $$FILE -run erlang halt ; \
	done

lang: subdirs_lang template
	@cp *.lang $(EBIN)
	@for FILE in $(TRANSLATABLE); do \
		for LANG_FILE in $(EBIN)/$$FILE\_*.lang; do \
			case $$LANG_FILE  in \
				$(EBIN)/$$FILE\_en.lang) ;; \
				*) echo "Processing: $$LANG_FILE"; \
	erl -pa $(WINGS_INTL) -noinput -eval "tools:diff(\"$$LANG_FILE\")" -run erlang halt >> $$LANG_FILE ;; \
			esac \
		done;\
	done

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
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
