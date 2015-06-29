#
#  Makefile --
#
#     Makefile for building Wings 3D.
#
#  Copyright (c) 2001-2013 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id$
#

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

include ../vsn.mk

ESRC=.
EBIN=.
ERLC=erlc

ifeq ($(TYPE),debug)
TYPE_FLAGS=-DDEBUG +debug_info
else
TYPE_FLAGS=+debug_info
endif

MODULES=tools

GEN_HEADER=wings_chars.hrl

TARGET_FILES= $(MODULES:%=$(EBIN)/%.beam) $(GEN_HEADER)

# Exclude generated wings_en.lang files
LANG_FILES= $(wildcard ../src/*[!en].lang)
#	$(wildcard ../plugins_src/*/*.lang)  takes to long...

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += 
ERL_COMPILE_FLAGS += -Werror $(TYPE_FLAGS) +debug_info

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt debug:
	$(MAKE) TYPE=$@ common

common: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f core

$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

$(GEN_HEADER): gen_char_hrl $(LANG_FILES)
	escript gen_char_hrl
