#
#  Makefile --
#
#     Makefile for building the plug-ins.
#
#  Copyright (c) 2001-2002 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.16 2006/01/17 23:22:01 dgud Exp $
#

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

ESRC=.
WINGS_INTL=../../intl_tools
EBIN=../../plugins/autouv
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
	auv_mapping \
	auv_matrix \
	auv_placement \
	auv_segment \
	auv_seg_ui \
	auv_texture \
	auv_util \
	wpc_snap \
	wpc_autouv


TARGET_FILES= $(MODULES:%=$(EBIN)/%.beam)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += -pa $(WINGS_EBIN) -pa $(WINGS_INTL) -I $(WINGS_SRC) -I $(WINGS_E3D) -pa $(ESDL_PATH)/ebin
ERL_COMPILE_FLAGS += -W $(TYPE_FLAGS) +warn_unused_vars +debug_info

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt debug:
	$(MAKE) TYPE=$@ common

common: $(TARGET_FILES) subdirs

subdirs: 
	(cd shaders; $(MAKE))

template: opt
	erl -pa $(WINGS_INTL) -noinput -run tools generate_template $(EBIN) -run erlang halt

lang: template
	@cp *.lang $(EBIN)
	@for i in $(EBIN)/autouv_*.lang; do \
		case $$i in \
			*/autouv_en.lang) ;; \
			*) echo "Processing: $$i"; \
	 erl -pa $(WINGS_INTL) -noinput -eval "tools:diff(\"$$i\")" -run erlang halt >> $$i ;; \
		esac \
	done

clean:
	rm -f $(TARGET_FILES)
	rm -f core

$(TARGET_FILES): auv.hrl

$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
