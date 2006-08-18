#
#  Makefile --
#
#     Makefile for copying shaders 
#
#  Copyright (c) 2006 Dan Gudmundsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.5 2006/01/27 15:17:56 dgud Exp $
#

DIR = ../../../plugins/autouv

FILES = standard.vs \
	wpc_noise.auv \
	noise.fs \
	wpc_wood.auv \
	wood.fs \
	wpc_filter.auv \
	image_filter.fs \
	README-shaders.txt


TARGETFILES = $(FILES:%=$(DIR)/%)

all: $(TARGETFILES) 

$(DIR)/%: ./%
	install -d $(DIR)
	install $< $@

clean:
	rm -f $(TARGETFILES)
	rm -f core

