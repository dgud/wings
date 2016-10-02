#
#  Makefile --
#
#     Makefile for building import/export plug-ins.
#
#  Copyright (c) 2001-2013 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.25 2006/09/06 22:52:18 antoneos Exp $
#

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

ESRC=.
WINGS_INTL=../../../intl_tools
#EBIN=D:/apps/wings3d205/lib/wings-2.0.5/plugins/import_export
EBIN=F:/apps/wings-2.0.5.73.ga4d1/lib/wings-2.0.5.73.ga4d1/plugins/import_export
ERLC=erlc
WINGS_SRC=../../../src
WINGS_EBIN=../../../ebin
WINGS_E3D=../../../e3d

ifeq ($(TYPE),debug)
TYPE_FLAGS=-DDEBUG
else
TYPE_FLAGS=
endif

MODULES= wpc_thebounty

TARGET_FILES= $(MODULES:%=$(EBIN)/%.beam)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -W3 \
  -pa $(WINGS_EBIN) -pa $(WINGS_INTL) -I $(WINGS_INTL) \
  -I $(WINGS_SRC) -I $(WINGS_E3D) $(TYPE_FLAGS) +debug_info

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt debug:
	$(MAKE) TYPE=$@ common

template: opt
	erl -pa $(WINGS_INTL) -noinput -run tools generate_template_files $(EBIN)

lang: template
	cp *.lang $(EBIN)
	erl -pa $(WINGS_INTL) -noinput -run tools diff_lang_files $(EBIN)

common: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f core

$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

# ----------------------------------------------------
# Dependencies
# ----------------------------------------------------

$(TARGET_FILES): $(WINGS_E3D)/e3d.hrl

