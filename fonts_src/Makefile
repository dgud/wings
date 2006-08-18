#
#  Makefile --
#
#     Makefile for fonts.
#
#  Copyright (c) 2001-2005 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.8 2005/06/08 05:58:17 bjorng Exp $
#

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

include ../vsn.mk

ESRC=.
EBIN=.
FONT_DIR=../fonts
ERLC=erlc

ifeq ($(TYPE),debug)
TYPE_FLAGS=-DDEBUG
else
TYPE_FLAGS=
endif

FONTS=       \
	6x11 \
	7x14 \
	fixed6x12 \
	fixed7x14

MODULES= \
	bdf2wingsfont

TARGET_FILES=  $(MODULES:%=$(EBIN)/%.beam) $(FONTS:%=$(FONT_DIR)/%.wingsfont)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -W $(TYPE_FLAGS) +debug_info '-Dwings_version="$(WINGS_VSN)"'

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt debug:
	$(MAKE) TYPE=$@ common

common: $(TARGET_FILES)


$(FONT_DIR)/6x11.wingsfont: 6x11_latin1.bdf 6x11_latin2.bdf 6x11_cyrillic.bdf

$(FONT_DIR)/7x14.wingsfont: 7x14_latin1.bdf 7x14_latin2.bdf 7x14_cyrillic.bdf

$(FONT_DIR)/fixed6x12.wingsfont: fixed6x12.bdf

$(FONT_DIR)/fixed7x14.wingsfont: fixed7x14.bdf


$(FONT_DIR)/*.wingsfont: bdf2wingsfont.beam


bdf2wingsfont.beam: ../vsn.mk

clean:
	rm -f $(TARGET_FILES)
	rm -f core

$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

$(FONT_DIR)/%.wingsfont:
	erl -noinput -run bdf2wingsfont convert $@ $(filter %.bdf,$^)
