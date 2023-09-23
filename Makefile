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

include erl.mk

# Check if OpenCL package is as external dependency
CL_PATH = $(shell $(ERL) -noshell -eval 'erlang:display(code:which(cl))' -s erlang halt)
ifneq (,$(findstring non_existing, $(CL_PATH)))
DEPS=cl
endif

DEPS += libigl eigen
SUBDIRS=c_src intl_tools src e3d plugins_src icons

#
# Normal build targets
#
opt debug clean:
	$(MAKE) TYPE=$@ common

common: $(SUBDIRS) deps_result

.PHONY: opt debug clean $(SUBDIRS) deps_result

$(SUBDIRS):
	$(MAKE) --directory=$@ $(TYPE)

#
# Build installer for Windows.
#
.PHONY: win32
win32: opt lang
	(cd win32; $(MAKE))
	escript tools/release

#
# Build a package for macOS.
#
.PHONY: macosx
macosx: macos
macos: opt lang
	escript tools/release

#
# Build package for Unix.
#
.PHONY: unix
unix: opt lang
	escript tools/release

#
# Generate language files
#
lang: intl_tools src plugins_src
	(cd src; $(MAKE) lang)
	(cd plugins_src; $(MAKE) lang)
	$(ESCRIPT) tools/verify_language_files .

#
# Build the source distribution.
#

.PHONY: .FORCE-WINGS-VERSION-FILE
vsn.mk: .FORCE-WINGS-VERSION-FILE
	@./WINGS-VERSION-GEN


#
# Dialyze wings and plugins (requires erlang-26)
#

.PHONY: dialyze
dialyze: opt intl_tools
	$(ESCRIPT) tools/dialyze.escript

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
# Build helper
#  Makes it possible to see 'OpenCL' build problems
deps_result: $(SUBDIRS)
	@cat _deps/build_log 2> /dev/null || true


# Internal dependencies
#   Some not needed, added to disable parallel builds of erlang dirs
#   which causes performance loss if an erlang build server is used
c_src: $(DEPS)
src: intl_tools e3d
plugins_src: intl_tools src
icons: e3d plugins_src
lang: opt

#
#  External Dependencies
#

CL_REPO = https://github.com/tonyrog/cl.git
IGL_REPO = https://github.com/dgud/libigl.git
EIGEN_REPO = https://github.com/eigenteam/eigen-git-mirror.git

CL_VER=cl-1.2.4
IGL_VER=master
EIGEN_VER=3.3.7
# see libigl/cmake/LibiglDownloadExternal.cmake for eigen version

GIT_FLAGS = -c advice.detachedHead=false clone --depth 1

.PHONY: cl igl eigen

# cl (erl wrapper library) not in path try to download and build it
cl: _deps/cl
	@(cd _deps/cl; rebar3 compile > ../build_log 2>&1 && rm ../build_log) \
	  || echo ***Warning*** OpenCL not useable >> _deps/build_log

_deps/cl:
	git $(GIT_FLAGS) -b $(CL_VER) $(CL_REPO) _deps/cl

# libigl have many useful function
libigl: _deps/libigl

_deps/libigl:
	git $(GIT_FLAGS) -b $(IGL_VER) $(IGL_REPO) _deps/libigl

# eigen needed by libigl
eigen: _deps/eigen

_deps/eigen:
	git $(GIT_FLAGS) -b $(EIGEN_VER) $(EIGEN_REPO) _deps/eigen
