#
#  Makefile --
#
#     Makefile for building
#
#  Copyright (c) 2001-2010 Bjorn Gustavsson Dan Gudmundsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

ESRC=.
WINGS_INTL=../../intl_tools
EBIN=../../plugins/pbr
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
	wpc_pbr \
	pbr_camera \
	pbr_cl \
	pbr_film \
	pbr_grid \
	pbr_hp \
	pbr_light \
	pbr_mat \
	pbr_mc \
	pbr_pathgpu \
	pbr_renderer \
	pbr_scene 

TARGET_FILES= $(MODULES:%=$(EBIN)/%.beam)

HEADER_FILES= pbr.hrl pbr_constants.hrl

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -pa $(WINGS_EBIN) -pa $(WINGS_INTL) -I $(WINGS_SRC) -I $(WINGS_E3D) -pa $(ESDL_PATH)/ebin -W $(TYPE_FLAGS) +debug_info

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt debug:
	mkdir -p $(EBIN)
	$(MAKE) TYPE=$@ common

common: $(TARGET_FILES) 

template: opt
	erl -pa $(WINGS_INTL) -noinput -run tools generate_template $(EBIN) -run erlang halt

lang: template
	cp *.lang $(EBIN)
	erl -pa $(WINGS_INTL) -noinput -run tools diff_lang_files $(EBIN)

clean:
	rm -f $(TARGET_FILES)
	rm -f core

$(TARGET_FILES): $(HEADER_FILES)

$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
