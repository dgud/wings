#
#  Makefile --
#
#     Makefile for fonts.
#
#  Copyright (c) 2001-2013 Bjorn Gustavsson
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
	fixed7x14 \
	Helvetica11 \
	Helvetica14 \
	Helvetica20 \
	Terminus16 \
	Terminus22 \
	CJK-12 \
	CJK-16 \
	CJK-efontB14 \
	CJK-efontB16 \
	CJK-efontB24


MODULES= \
	bdf2wingsfont

FONT_FILES= $(FONTS:%=$(FONT_DIR)/%.wingsfont)
TARGET_FILES=  $(MODULES:%=$(EBIN)/%.beam) $(FONT_FILES)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------
ERL_COMPILE_FLAGS += -Werror $(TYPE_FLAGS) +debug_info

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

opt debug:
	$(MAKE) TYPE=$@ common

common: $(TARGET_FILES)


$(FONT_DIR)/6x11.wingsfont: 6x11_latin1.bdf 6x11_latin2.bdf 6x11_cyrillic.bdf 6x11_latin5.bdf
$(FONT_DIR)/7x14.wingsfont: 7x14_latin1.bdf 7x14_latin2.bdf 7x14_cyrillic.bdf 7x14_latin5.bdf

$(FONT_DIR)/fixed6x12.wingsfont: fixed6x12.bdf
$(FONT_DIR)/fixed7x14.wingsfont: fixed7x14.bdf

# helvR??.bdf are sourced from font-adobe-100dpi-1.0.1.tar.gz
# crox?h.bdf are sourced from font-cronyx-cyrillic-1.0.1.tar.gz
#
# Both tarballs were downloaded from
# http://xorg.freedesktop.org/releases/individual/font/
#
$(FONT_DIR)/Helvetica11.wingsfont: helvR08.bdf helvR08_char0.bdf crox1h.bdf
$(FONT_DIR)/Helvetica14.wingsfont: helvR10.bdf helvR10_char0.bdf crox2h.bdf
$(FONT_DIR)/Helvetica20.wingsfont: helvR14.bdf helvR14_char0.bdf crox4h.bdf

# ter-u???.bdf are sourced from terminus-font-4.30.tar.gz
# downloaded from http://www.is-vn.bg/hamster/
#
$(FONT_DIR)/Terminus16.wingsfont: ter-u16b.bdf
$(FONT_DIR)/Terminus22.wingsfont: ter-u22b.bdf

# wenquanyi_*pt_cjk.bdf are built from wqy-bitmapsong-bdf-1.0.0-RC1.tar.gz
# downloaded from https://sourceforge.net/projects/wqy/
#
# These BDF files are built with the command
#
#	make -f Makefile_liber wenquanyi_{9,10,11,12}pt_cjk.bdf
#
# gulim12.bdf sourced from xfonts-baekmuk_2.2.orig.tar.gz
# downloaded from http://ftp.us.debian.org/debian/pool/main/x/xfonts-baekmuk/
#
#
$(FONT_DIR)/CJK-12.wingsfont: gulim12.bdf wenquanyi_10pt_cjk.bdf wenquanyi_10pt_cjk_char0.bdf
$(FONT_DIR)/CJK-16.wingsfont: wenquanyi_12pt_cjk.bdf wenquanyi_12pt_cjk_char0.bdf

# efont_b???.bdf sourced from efont-unicode-bdf-0.4.2.tar.bz2
# downloaded from http://openlab.jp/efont/unicode/index.html.en
# 
#
$(FONT_DIR)/CJK-efontB14.wingsfont: efont_b14.bdf efont_16pt_cjk_char0.bdf
$(FONT_DIR)/CJK-efontB16.wingsfont: efont_b16.bdf efont_16pt_cjk_char0.bdf
$(FONT_DIR)/CJK-efontB24.wingsfont: efont_b24.bdf efont_24pt_cjk_char0.bdf



$(FONT_FILES): bdf2wingsfont.beam

bdf2wingsfont.beam: font_version.hrl

clean:
	rm -f $(TARGET_FILES)
	rm -f $(FONT_DIR)/*.wingsfont
	rm -f core

$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

$(FONT_DIR)/%.wingsfont:
	erl -noinput -run bdf2wingsfont convert $@ $(filter %.bdf,$^)
