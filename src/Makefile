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
WINGS_INTL=../intl_tools
EBIN=../ebin
ERLC=erlc

ifeq ($(TYPE),debug)
TYPE_FLAGS=-DDEBUG +debug_info
else
TYPE_FLAGS=+debug_info
endif

# All modules except wings_langs are mentioned here. It contains a parse
# transform needed for compiling the other modules, so we must use a special
# rule to ensure that it gets compiled before all other modules.

MODULES= \
	user_default \
	wings \
	wings_align \
	wings_body \
	wings_bool \
	wings_camera \
	wings_cl \
	wings_cc \
	wings_collapse \
	wings_color \
	wings_console \
	wings_deform \
	wings_develop \
	wings_dialog \
	wings_dissolve \
	wings_dl \
	wings_drag \
	wings_draw \
	wings_draw_setup \
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
	wings_frame \
	wings_geom_win \
	wings_gl \
	wings_glfont \
	wings_help \
	wings_hotkey \
	wings_info \
	wings_image \
	wings_image_viewer \
	wings_import \
	wings_io \
	wings_io_wx \
	wings_job \
	wings_light \
	wings_magnet \
	wings_material \
	wings_menu \
	wings_menu_util \
	wings_move \
	wings_msg \
	wings_obj \
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
	wings_shaders \
	wings_shapes \
	wings_start \
	wings_status \
	wings_subdiv \
	wings_sup \
	wings_theme \
	wings_toolbar \
	wings_tesselation \
	wings_text \
	wings_tweak \
	wings_tweak_win \
	wings_u \
	wings_undo \
	wings_util \
	wings_va \
	wings_vbo \
	wings_vec \
	wings_vertex \
	wings_vertex_cmd \
	wings_view \
	wings_we \
	wings_we_build \
	wings_we_util \
	wings_wm \
	wpa \
	ww_color_ctrl \
	ww_color_slider \
	jsone \
	jsone_decode \
	jsone_encode

TARGET_FILES= $(MODULES:%=$(EBIN)/%.beam)

WINGS_MODS= $(MODULES:%=%,)wings_lang

APP_FILE   = wings.app
APP_SRC    = $(APP_FILE).src
APP_TARGET = $(EBIN)/$(APP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -Werror -I ../.. -I../_deps $(TYPE_FLAGS) \
'-Dwings_version="$(WINGS_VSN)"' -pa $(WINGS_INTL)

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt debug wx:
	$(MAKE) TYPE=$@ common

common: $(EBIN)/wings_lang.beam $(TARGET_FILES) $(APP_TARGET)

template: $(TARGET_FILES)
	erl -pa $(WINGS_INTL) -noinput -run tools generate_template ../ebin -run erlang halt

lang: template
	@cp *.lang $(EBIN)
	erl -pa $(WINGS_INTL) -noinput -run tools diff_lang_files $(EBIN)

clean:
	rm -f $(TARGET_FILES) $(EBIN)/wings_lang.beam
	rm -f core

$(EBIN)/wings_u.beam: $(ESRC)/wings_u.erl
	$(ERLC) '-Dwings_branch="$(WINGS_BRANCH)"' $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

$(APP_TARGET): $(APP_SRC) ../vsn.mk Makefile
	$(gen_verbose)sed -e 's;%WINGS_MODS%;$(WINGS_MODS);' $< > $@.tmp
	$(vsn_verbose)sed -e 's;%VSN%;$(WINGS_VSN);' $@.tmp > $@
	$(V_at)rm $@.tmp

# ----------------------------------------------------
# Dependencies
# ----------------------------------------------------
$(EBIN)/wings_export.beam $(EBIN)/wings_file.beam $(EBIN)/wings_help.beam $(EBIN)/wings_pref.beam $(EBIN)/wings_u.beam $(EBIN)/wpa.beam: ../vsn.mk

$(EBIN)/wings_text.beam: ../intl_tools/wings_chars.hrl

$(TARGET_FILES): $(EBIN)/wings_lang.beam wings.hrl

DEPENDENT_OF_E3D_HRL = \
	wings_draw \
	wings_export \
	wings_face \
	wings_file \
	wings_import \
	wings_job \
	wings_light \
	wings_pick \
	wings_plugin \
	wings_tesselation \
	wings_we \
	wpa

$(DEPENDENT_OF_E3D_HRL:%=$(EBIN)/%.beam): ../e3d/e3d.hrl
