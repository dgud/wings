#
#  Makefile --
#
#     Makefile for building AutoUV.
#
#  Copyright (c) 2001-2013 Bjorn Gustavsson
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
WINGS_TOP=../../..

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
	wpc_snap_win \
	wpc_autouv


TARGET_FILES= $(MODULES:%=$(EBIN)/%.beam)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -Werror -pa $(WINGS_INTL) -I $(WINGS_TOP) $(TYPE_FLAGS) +debug_info

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
	cp *.lang $(EBIN)
	erl -pa $(WINGS_INTL) -noinput -run tools diff_lang_files $(EBIN)

clean:
	rm -f $(TARGET_FILES)
	rm -f core

$(TARGET_FILES): auv.hrl

$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
