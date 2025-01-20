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
	wpc_scripting_shapes \
	scripting_engines

INIT_SCRIPT_SRC=$(ESRC)/init_scripts
INIT_SCRIPT_EBIN=$(EBIN)/wpc_scripting_shapes_init
INIT_SCRIPT_PY_SRC=$(INIT_SCRIPT_SRC)/py
INIT_SCRIPT_PY_EBIN=$(INIT_SCRIPT_EBIN)/py
INIT_SCRIPT_SCM_SRC=$(INIT_SCRIPT_SRC)/scm
INIT_SCRIPT_SCM_EBIN=$(INIT_SCRIPT_EBIN)/scm

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

common: $(TARGET_FILES) $(INIT_SCRIPT_EBIN) subdirs

subdirs:
	(cd docs; $(MAKE))

clean:
	rm -f $(TARGET_FILES)
	rm -f core
	(cd docs; $(MAKE) clean)

SH?=sh

$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

PY_FILES= init w3d_e3d w3d_int w3d_newshape
PY_FILES_1=$(PY_FILES:%=$(INIT_SCRIPT_PY_EBIN)/%.py)

SCM_FILES= init_env_csi init_env_gauche
SCM_FILES_1=$(SCM_FILES:%=$(INIT_SCRIPT_SCM_EBIN)/%.scm)

$(INIT_SCRIPT_EBIN): $(INIT_SCRIPT_EBIN)/callable.conf \
                     $(INIT_SCRIPT_EBIN)/defaults.conf \
                     $(INIT_SCRIPT_EBIN)/python.script-init-conf \
                     $(INIT_SCRIPT_EBIN)/scheme.script-init-conf \
                     $(INIT_SCRIPT_PY_EBIN)/w3d_we.py \
                     $(INIT_SCRIPT_SCM_EBIN)/init.scm \
                     $(PY_FILES_1) \
                     $(SCM_FILES_1)


$(INIT_SCRIPT_EBIN)/%.conf: $(INIT_SCRIPT_SRC)/%.conf
	cp $< $(INIT_SCRIPT_EBIN)/
$(INIT_SCRIPT_EBIN)/%.script-init-conf: $(INIT_SCRIPT_SRC)/%.script-init-conf
	cp $< $(INIT_SCRIPT_EBIN)/
$(INIT_SCRIPT_PY_EBIN)/%.py: $(INIT_SCRIPT_PY_SRC)/%.py
	cp $< $(INIT_SCRIPT_PY_EBIN)/
$(INIT_SCRIPT_SCM_EBIN)/%.scm: $(INIT_SCRIPT_SCM_SRC)/%.scm
	cp $< $(INIT_SCRIPT_SCM_EBIN)/

# Generate w3d_we.py file from callable.conf
$(INIT_SCRIPT_PY_EBIN)/w3d_we.py: $(INIT_SCRIPT_PY_SRC)/w3d_we.1.py \
                                  $(INIT_SCRIPT_SRC)/callable.conf \
                                  $(INIT_SCRIPT_SRC)/py-modnames
	$(SH) ./tools/gen-init-we-script.sh \
		"$(INIT_SCRIPT_PY_SRC)/w3d_we.1.py" \
		- \
		"$(INIT_SCRIPT_SRC)/callable.conf" \
		"$(INIT_SCRIPT_SRC)/py-modnames" py \
		> "$(INIT_SCRIPT_PY_EBIN)/w3d_we.py"

# Generate init.scm file from callable.conf
$(INIT_SCRIPT_SCM_EBIN)/init.scm: $(INIT_SCRIPT_SCM_SRC)/init.1.scm \
                                  $(INIT_SCRIPT_SRC)/callable.conf \
                                  $(INIT_SCRIPT_SRC)/scm-modnames \
                                  $(INIT_SCRIPT_SCM_SRC)/init.2.scm
	$(SH) ./tools/gen-init-we-script.sh \
		"$(INIT_SCRIPT_SCM_SRC)/init.1.scm" \
		"$(INIT_SCRIPT_SCM_SRC)/init.2.scm" \
		"$(INIT_SCRIPT_SRC)/callable.conf" \
		"$(INIT_SCRIPT_SRC)/scm-modnames" scm \
		> "$(INIT_SCRIPT_SCM_EBIN)/init.scm"

