#
#  Makefile --
#
#     Makefile for building icons for Wings 3D.
#
#  Copyright (c) 2001 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.6 2006/08/01 15:19:56 giniu Exp $
#

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

ESRC=.
EBIN=../ebin
ERLC=erlc

ICONSETS= \
	classic \
	bluecube

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += 
ERL_COMPILE_FLAGS += -W

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: collect_bmp.erl */*.bmp */*.tga
	$(ERLC) $(ERL_COMPILE_FLAGS) collect_bmp.erl
	@for ICONSET in $(ICONSETS); do \
		erl $(ERL_FLAGS) -noinput -run collect_bmp start $(ESRC)/$$ICONSET $(EBIN)/wings_icon_$$ICONSET.bundle -s erlang halt; \
	done

clean:
	@for ICONSET in $(ICONSETS); do \
		rm -f $(EBIN)/wings_icon_$$ICONSET.bundle; \
	done
	rm -f collect_bmp.beam
