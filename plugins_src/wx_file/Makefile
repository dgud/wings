#
#  Makefile --
#
#     Makefile for building file dialog plug-in for Windows.
#
#  Copyright (c) 2003-2013 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id$
#

WINGS_INTL=../../intl_tools
LIBDIR = ../../plugins/wx_file

BEAM = $(LIBDIR)/wp8_file.beam

ERLC = erlc
ERLC_FLAGS = -o $(LIBDIR) -I ../../e3d -I ../../src -Werror +debug_info -pa $(WINGS_INTL) -I $(WINGS_INTL) -DUSE_WX

opt: $(BEAM) 

template: opt
	erl -pa $(WINGS_INTL) -noinput -run tools generate_template_files $(LIBDIR)

lang: template
	cp *.lang $(LIBDIR)
	erl -pa $(WINGS_INTL) -noinput -run tools diff_lang_files $(LIBDIR)

$(LIBDIR)/%.beam: %.erl
	install -d $(LIBDIR)
	$(ERLC) $(ERLC_FLAGS) $<

