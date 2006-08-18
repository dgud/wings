#
#  Makefile --
#
#     Makefile for building the plug-ins.
#
#  Copyright (c) 2001-2003 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.21 2006/06/29 19:57:58 giniu Exp $
#

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

ESRC=.
WINGS_INTL=../../intl_tools
EBIN=../../plugins/commands
ERLC=erlc
WINGS_SRC=../../src
WINGS_EBIN=../../ebin
WINGS_E3D=../../e3d

ifeq ($(TYPE),debug)
TYPE_FLAGS=-DDEBUG
else
TYPE_FLAGS=
endif

MODULES= \
	wpc_absolute_move \
	wpc_absolute_scale \
	wpc_bend \
	wpc_connect_tool \
	wpc_doosabin \
	wpc_intersect_edge \
	wpc_intersect_vertex \
	wpc_region \
	wpc_shift \
	wpc_turnedge \
	wpc_tweak \
	wpc_untriangulate \
	wpc_weld

TRANSLATABLE= \
	wpc_absolute_move \
	wpc_absolute_scale \
	wpc_bend \
	wpc_connect_tool \
	wpc_doosabin \
	wpc_intersect_edge \
	wpc_intersect_vertex \
	wpc_region \
	wpc_shift \
	wpc_turnedge \
	wpc_tweak \
	wpc_untriangulate \
	wpc_weld

TARGET_FILES= $(MODULES:%=$(EBIN)/%.beam)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += -pa $(WINGS_EBIN) -I $(WINGS_SRC) -I $(WINGS_E3D) -pa $(ESDL_PATH)/ebin
ERL_COMPILE_FLAGS += -W $(TYPE_FLAGS) +warn_unused_vars +debug_info -pa $(WINGS_INTL) -I $(WINGS_INTL)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt debug:
	$(MAKE) TYPE=$@ common

template: opt
	@for FILE in $(TRANSLATABLE); do \
		erl -pa $(WINGS_INTL) -noinput -run tools generate_template_file $(EBIN) $$FILE -run erlang halt ; \
	done

lang: template
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

common: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f core

$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
