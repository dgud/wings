#
#  Makefile --
#
#     Makefile for building the E3D modules.
#
#  Copyright (c) 2001-2005 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.15 2005/06/20 20:22:01 dgud Exp $
#

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

ESRC=.
EBIN=../ebin
ERLC=erlc

ifeq ($(TYPE),debug)
TYPE_FLAGS=-DDEBUG +debug_info
else
TYPE_FLAGS= +debug_info
endif

MODULES= \
	e3d_image \
	e3d_mesh \
	e3d_file \
	e3d_obj \
	e3d_tds \
	e3d_mat \
	e3d_util \
	e3d_vec \
	e3d_q \
	e3d_bv \
	e3d__tri_quad \
	e3d__meshclean \
	e3d__bmp \
	e3d__tga \
	e3d__png \
	e3d__tif

TARGET_FILES= $(MODULES:%=$(EBIN)/%.beam)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += 
ERL_COMPILE_FLAGS += -W $(TYPE_FLAGS) +warn_unused_vars

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
