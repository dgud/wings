#
#  Makefile --
#
#     Makefile for building Wings 3D.
#
#  Copyright (c) 2001-2005 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.62 2005/09/13 09:15:58 giniu Exp $
#

.SUFFIXES: .erl .jam .beam .yrl .xrl .bin .mib .hrl .sgml .html .ps .3 .1 \
	.fig .dvi .tex .class .java .pdf .psframe .pscrop

include ../vsn.mk

ESRC=.
WINGS_INTL=../intl_tools
EBIN=../ebin
ERLC=erlc

ifeq ($(TYPE),debug)
TYPE_FLAGS=-DDEBUG +debug_info
else
TYPE_FLAGS=+debug_info
endif

# Note: wings_lang must be compiled before other modules -
# it contains a parse transform.
MODULES= \
	wings_lang \
	user_default \
	wings \
	wings_align \
	wings_ask \
	wings_body \
	wings_camera \
	wings_collapse \
	wings_color \
	wings_console \
	wings_deform \
	wings_dissolve \
	wings_dl \
	wings_drag \
	wings_draw \
	wings_draw_util \
	wings_edge \
	wings_edge_cmd \
	wings_edge_loop \
	wings_export \
	wings_extrude_edge \
	wings_extrude_face \
	wings_face \
	wings_face_cmd \
	wings_facemat \
	wings_file \
	wings_ff_ndo \
	wings_ff_wings \
	wings__font \
	wings_gl \
	wings_help \
	wings_hotkey \
	wings_image \
	wings_import \
	wings_init \
	wings_io \
	wings_job \
	wings_light \
	wings_magnet \
	wings_material \
	wings_menu \
	wings_menu_util \
	wings_move \
	wings_msg \
	wings_outliner \
	wings_palette \
	wings_pb \
	wings_pick \
	wings_plugin \
	wings_pref \
	wings_pref_dlg \
	wings_proxy \
	wings_render \
	wings_rotate \
	wings_s \
	wings_sel \
	wings_sel_conv \
	wings_sel_cmd \
	wings_scale \
	wings_shape \
	wings_shapes \
	wings_start \
	wings_subdiv \
	wings_toolbar \
	wings_tesselation \
	wings_text \
	wings_u \
	wings_undo \
	wings_util \
	wings_vec \
	wings_vertex \
	wings_vertex_cmd \
	wings_view \
	wings_we \
	wings_we_util \
	wings_wm \
	wings_wm_toplevel \
	wings__du \
	wpa


TARGET_FILES= $(MODULES:%=$(EBIN)/%.beam)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_FLAGS += -pa $(ESDL_PATH)/ebin +warn_unused_vars
ERL_COMPILE_FLAGS += -I$(ESDL_PATH)/include -I ../e3d -W $(TYPE_FLAGS) \
 '-Dwings_version="$(WINGS_VSN)"' -pa $(WINGS_INTL)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt debug:
	$(MAKE) TYPE=$@ common

common: $(TARGET_FILES)

template: opt
	erl -pa $(WINGS_INTL) -noinput -run tools generate_template ../ebin -run erlang halt

lang: template
	@cp *.lang $(EBIN)
	@for i in $(EBIN)/wings_*.lang; do \
		case $$i in \
			*/wings_en.lang) ;; \
			*) echo "Processing: $$i"; \
	 erl -pa $(WINGS_INTL) -noinput -eval "tools:diff(\"$$i\")" -run erlang halt >> $$i ;; \
		esac \
	done

clean:
	rm -f $(TARGET_FILES)
	rm -f core

$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

$(TARGET_FILES): wings.hrl

$(EBIN)/wings_file.beam $(EBIN)/wings_help.beam $(EBIN)/wings_text.beam: ../vsn.mk
