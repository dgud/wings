#
#  Makefile --
#
#     Makefile for building the plug-ins.
#
#  Copyright (c) 2001-2004 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.25 2006/09/06 22:52:18 antoneos Exp $
#

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

ESRC=.
WINGS_INTL=../../intl_tools
EBIN=../../plugins/import_export
ERLC=erlc
WINGS_SRC=../../src
WINGS_EBIN=../../ebin
WINGS_E3D=../../e3d
ESDL=$(ESDL_PATH)

ifeq ($(TYPE),debug)
TYPE_FLAGS=-DDEBUG
else
TYPE_FLAGS=
endif

MODULES= \
	wpc_ai \
	wpc_3ds \
	wpc_obj \
	wpc_opengl \
	wpc_opengl_sl \
	wpc_rwx \
	wpc_wrl \
	wpc_hlines \
	wpc_toxic \
	wpc_stl \
	wpc_x \
	wpc_lwo \
	wpc_yafray

TRANSLATABLE= \
	wpc_ai \
	wpc_3ds \
	wpc_obj \
	wpc_opengl \
	wpc_rwx \
	wpc_wrl \
	wpc_hlines \
	wpc_toxic \
	wpc_yafray

TARGET_FILES= $(MODULES:%=$(EBIN)/%.beam)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += -pa $(WINGS_EBIN) -pa $(WINGS_INTL) -I $(WINGS_INTL) -I $(WINGS_SRC) -I $(WINGS_E3D) -I $(ESDL_PATH)/include
ERL_COMPILE_FLAGS += -W $(TYPE_FLAGS) -pa $(ESDL_PATH)/ebin +warn_unused_vars +debug_info

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt debug:
	$(MAKE) TYPE=$@ common

template: opt
	@for FILE in $(TRANSLATABLE); do \
		erl -pa $(WINGS_INTL) -noinput -run tools generate_template_file $(EBIN) $$FILE -run erlang halt ; \
	done

lang: template
	@cp *.lang $(EBIN)
	@for FILE in $(TRANSLATABLE); do \
		for LANG_FILE in $(EBIN)/$$FILE\_*.lang; do \
			case $$LANG_FILE  in \
				$(EBIN)/$$FILE\_en.lang) ;; \
				*) echo "Processing: $$LANG_FILE"; \
	erl -pa $(WINGS_INTL) -noinput -eval "tools:diff(\"$$LANG_FILE\")" -run erlang halt >> $$LANG_FILE ;; \
			esac \
		done;\
	done

common: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f core

$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
