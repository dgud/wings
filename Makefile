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

# Check if OpenCL package is needed
ifeq ($(WSLcross), true)
ERL = erl.exe
else
ERL = erl
endif

CL_PATH = $(shell $(ERL) -noshell -eval 'erlang:display(code:which(cl))' -s erlang halt)
ifneq (,$(findstring non_existing, $(CL_PATH)))
DEPS = cl
CL_VER=cl-1.2.4
endif

.PHONY: all debug clean lang

all: $(DEPS)
	(cd intl_tools; $(MAKE))
	(cd src; $(MAKE))
	(cd e3d; $(MAKE))
	(cd plugins_src; $(MAKE))
	(cd icons; $(MAKE))
	@cat _deps/build_log 2> /dev/null || true

debug: $(DEPS)
	(cd intl_tools; $(MAKE) debug)
	(cd src; $(MAKE) debug)
	(cd e3d; $(MAKE) debug)
	(cd plugins_src; $(MAKE) debug)
	(cd icons; $(MAKE) debug)
	@cat _deps/build_log 2> /dev/null || true

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

#
# cl (erl wrapper library) not in path try to download and build it
#

CL_REPO = https://github.com/tonyrog/cl.git

.PHONY: cl
cl: _deps/cl
	@(cd _deps/cl; rebar3 compile > ../build_log 2>&1 && rm ../build_log) \
	  || echo ***Warning*** OpenCL not useable >> _deps/build_log

_deps/cl:
	git -c advice.detachedHead=false clone --depth 1 -b $(CL_VER) $(CL_REPO) _deps/cl
