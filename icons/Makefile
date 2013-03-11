#
#  Makefile --
#
#     Makefile for building icons for Wings 3D.
#
#  Copyright (c) 2001-2013 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.7 2006/08/23 02:56:49 antoneos Exp $
#

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

ESRC=.
EBIN=../ebin
ERLC=erlc

ICONSETS= \
	classic \
	bluecube \
	purpletube

TARGET_FILES = $(ICONSETS:%=$(EBIN)/wings_icon_%.bundle)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += 
ERL_COMPILE_FLAGS += -Werror +debug_info

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt debug: collect_bmp.beam $(TARGET_FILES)

$(EBIN)/wings_icon_%.bundle: ${wildcard %/*.bmp} ${wildcard %/*.tga} collect_bmp.beam about_wings.bmp
	erl $(ERL_FLAGS) -noinput -run collect_bmp start . $(ESRC)/$* $@ -s erlang halt

clean:
	for ICONSET in $(ICONSETS); do \
		rm -f $(EBIN)/wings_icon_$$ICONSET.bundle; \
	done
	rm -f collect_bmp.beam

%.beam: $(ESRC)/%.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) -o. $<
