#
#  Makefile --
#
#     Makefile for building plug-ins for scripting.
#
#  Copyright (c) 2001-2013 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.14 2006/08/02 22:44:40 antoneos Exp $
#
include ../../erl.mk

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

ESRC=.
WINGS_INTL=../../intl_tools
EBIN=../../plugins/scripting
WINGS_TOP=../../..
WINGS_E3D=../../e3d

ifeq ($(TYPE),debug)
TYPE_FLAGS=-DDEBUG
else
TYPE_FLAGS=
endif

MODULES= \
	wpc_scripting_shapes

TARGET_FILES= $(MODULES:%=$(EBIN)/%.beam)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -Werror +nowarn_match_float_zero -I $(WINGS_TOP) \
	$(TYPE_FLAGS) -pa $(WINGS_INTL) +debug_info
# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt debug:
	$(MAKE) TYPE=$@ common

template: opt
	$(ERL) -pa $(WINGS_INTL) -noinput -run tools generate_template_files $(EBIN)

lang: template
	cp *.lang $(EBIN)
	$(ERL) -pa $(WINGS_INTL) -noinput -run tools diff_lang_files $(EBIN)

common: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f core

$(EBIN)/%.beam: $(ESRC)/%.erl $(EBIN)/wpc_scripting_shapes_init
	$(ERLC) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
	
$(EBIN)/wpc_scripting_shapes_init: $(EBIN)/wpc_scripting_shapes_init/init.py \
                                   $(EBIN)/wpc_scripting_shapes_init/w3d_e3d.py \
								   $(EBIN)/wpc_scripting_shapes_init/w3d_int.py \
								   $(EBIN)/wpc_scripting_shapes_init/w3d_newshape.py \
								   $(EBIN)/wpc_scripting_shapes_init/init.scm \
								   $(EBIN)/wpc_scripting_shapes_init/init_env_csi.scm \
								   $(EBIN)/wpc_scripting_shapes_init/init_env_gauche.scm \

MKDIR_INIT_SCRIPTS=mkdir -p $(EBIN)/wpc_scripting_shapes_init/
$(EBIN)/wpc_scripting_shapes_init/init.py: $(ESRC)/init_scripts/init.py
	$(MKDIR_INIT_SCRIPTS)
	cp $(ESRC)/init_scripts/init.py $(EBIN)/wpc_scripting_shapes_init/
$(EBIN)/wpc_scripting_shapes_init/w3d_e3d.py: $(ESRC)/init_scripts/w3d_e3d.py
	cp $(ESRC)/init_scripts/w3d_e3d.py $(EBIN)/wpc_scripting_shapes_init/
$(EBIN)/wpc_scripting_shapes_init/w3d_int.py: $(ESRC)/init_scripts/w3d_int.py
	cp $(ESRC)/init_scripts/w3d_int.py $(EBIN)/wpc_scripting_shapes_init/
$(EBIN)/wpc_scripting_shapes_init/w3d_newshape.py: $(ESRC)/init_scripts/w3d_newshape.py
	cp $(ESRC)/init_scripts/w3d_newshape.py $(EBIN)/wpc_scripting_shapes_init/

$(EBIN)/wpc_scripting_shapes_init/init.scm: $(ESRC)/init_scripts/init.scm
	cp $(ESRC)/init_scripts/init.scm $(EBIN)/wpc_scripting_shapes_init/
$(EBIN)/wpc_scripting_shapes_init/init_env_csi.scm: $(ESRC)/init_scripts/init_env_csi.scm
	cp $(ESRC)/init_scripts/init_env_csi.scm $(EBIN)/wpc_scripting_shapes_init/
$(EBIN)/wpc_scripting_shapes_init/init_env_gauche.scm: $(ESRC)/init_scripts/init_env_gauche.scm
	cp $(ESRC)/init_scripts/init_env_gauche.scm $(EBIN)/wpc_scripting_shapes_init/


#$(EBIN)/wpc_scripting_shapes.beam: $(WINGS_E3D)/e3d.hrl
