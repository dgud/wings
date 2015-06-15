#
#  Makefile --
#
#     Top-level Makefile for building Wings 3D.
#
#  Coepyright (c) 2001-2011 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
.PHONY: all debug clean lang

all:
	(cd intl_tools; $(MAKE))
	(cd src; $(MAKE))
	(cd e3d; $(MAKE))
	(cd plugins_src; $(MAKE))
	(cd icons; $(MAKE))

debug:
	(cd intl_tools; $(MAKE) debug)
	(cd src; $(MAKE) debug)
	(cd e3d; $(MAKE) debug)
	(cd plugins_src; $(MAKE) debug)
	(cd icons; $(MAKE) debug)

clean:
	(cd intl_tools; $(MAKE) clean)
	(cd src; $(MAKE) clean)
	(cd e3d; $(MAKE) clean)
	(cd plugins_src; $(MAKE) clean)
	(cd icons; $(MAKE) clean)

lang: all
	(cd intl_tools; $(MAKE))
	(cd src; $(MAKE) lang)
	(cd plugins_src; $(MAKE) lang)
	escript tools/verify_language_files .

#
# Build installer for Windows.
#
.PHONY: win32
win32: all lang
	(cd win32; $(MAKE))
	escript tools/release

#
# Build a package for MacOS X.
#
.PHONY: macosx
macosx: all lang
	escript tools/release

#
# Build package for Unix.
#
.PHONY: unix
unix: all lang
	escript tools/release

#
# Build the source distribution.
#

.PHONY: .FORCE-WINGS-VERSION-FILE
vsn.mk: .FORCE-WINGS-VERSION-FILE
	@./WINGS-VERSION-GEN

-include vsn.mk

WINGS_TARNAME=wings-$(WINGS_VSN)
.PHONY: dist
dist:
	git archive --format=tar \
		--prefix=$(WINGS_TARNAME)/ HEAD^{tree} > $(WINGS_TARNAME).tar
	@mkdir -p $(WINGS_TARNAME)
	@echo $(WINGS_VSN) > $(WINGS_TARNAME)/version
	tar rf $(WINGS_TARNAME).tar $(WINGS_TARNAME)/version
	@rm -r $(WINGS_TARNAME)
	bzip2 -f -9 $(WINGS_TARNAME).tar
