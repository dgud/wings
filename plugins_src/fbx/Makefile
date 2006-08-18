#
#  Makefile --
#
#     Makefile for building the FBX plug-in.
#
#  Copyright (c) 2003-2005 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.7 2006/01/20 14:47:02 giniu Exp $
#
FBXSDK = FBXSDK60
LIBDIR=../../plugins/fbx
WINGS_INTL=../../intl_tools

OS := $(shell uname -s)

ifneq (,$(findstring CYGWIN,$(OS)))
  FBX_SDK := c:\Program\Alias\$(FBXSDK)
  ERL_TOP := $(shell which erl)
  ERL_TOP := $(subst /bin/erl,,$(ERL_TOP))
  ERL_TOP := $(shell cygpath -w $(ERL_TOP))
  DRIVER_NAME := wings_fbx_win32
  DRIVER_FILE := $(DRIVER_NAME).dll
  WINGS_E3D := $(shell cygpath -m $(HOME)/wings/e3d)
  MLFLAGS = /nologo /subsystem:console /machine:I386
# /nodefaultlib 
#  MLIBS = kernel32.lib user32.lib advapi32.lib oldnames.lib libcmt.lib msvcrt.lib $(FBX_SDK)\lib\fbxsdk_mt.lib $(FBX_SDK)\lib\zlib_mt.lib
  MLIBS = advapi32.lib $(FBX_SDK)\lib\fbxsdk_mt.lib $(FBX_SDK)\lib\zlib_mt.lib
  CC = cl -nologo -MT
  CXX = $(CC)
  OBJ = obj
  OS_CFLAGS = -DNDEBUG -DWIN32 -D_CONSOLE -D_MBCS -DK_PLUGIN -DK_FBXSDK
else
  ifeq ($(OS),Linux)
   FBX_SDK := /usr/local/$(FBXSDK)
   DRIVER_NAME := wings_fbx_linux
   OS_LDFLAGS = -shared
   STRIP_FLAGS = --strip-debug 
  else
  # Must be Darwin
   FBX_SDK := /Applications/$(FBXSDK)
   DRIVER_NAME := wings_fbx_mac
   OS_LDFLAGS = -bundle -flat_namespace -undefined suppress
   STRIP_FLAGS = -S
   EXTRA_INCLUDES := -I/usr/include/sys
   CXX = /usr/bin/g++-3.3
  endif
  OS_CFLAGS = -O2 -fPIC
  ERL_TOP := /usr/local/lib/erlang
  DRIVER_FILE := $(DRIVER_NAME).so
  WINGS_E3D := $(HOME)/wings/e3d
  OBJ = o
endif

INCLUDES = -I. -I$(ERL_TOP)/usr/include -I$(FBX_SDK)/include $(EXTRA_INCLUDES)
LIB_DIR = $(FBX_SDK)/lib
CFLAGS = $(INCLUDES) -DWINGS_FBX_DRIVER_NAME='"$(DRIVER_NAME)"' $(OS_CFLAGS)

LD_FLAGS = $(OS_LDFLAGS) $(LIBS)
LIBS = -L$(LIB_DIR) -lfbxsdk

ERLC = erlc
ERLC_FLAGS = -I$(WINGS_E3D) -o $(LIBDIR) +debug_info -pa $(WINGS_INTL) -I $(WINGS_INTL)

OBJS = wings_fbx_driver.$(OBJ) wings_fbx.$(OBJ)

opt: $(LIBDIR)/wpc_fbx.beam $(LIBDIR)/$(DRIVER_FILE)

template: opt
	erl -pa $(WINGS_INTL) -noinput -run tools generate_template_file $(LIBDIR) wpc_fbx -run erlang halt

lang: template
	@cp *.lang $(LIBDIR)
	@for LANG_FILE in $(LIBDIR)/wpc_fbx_*.lang; do \
		case $$LANG_FILE  in \
			$(LIBDIR)/wpc_fbx_en.lang) ;; \
			*) echo "Processing: $$LANG_FILE"; \
	erl -pa $(WINGS_INTL) -noinput -eval "tools:diff(\"$$LANG_FILE\")" -run erlang halt >> $$LANG_FILE ;; \
		esac \
	done

$(LIBDIR)/$(DRIVER_NAME).so: $(OBJS)
	$(CXX) -o $(LIBDIR)/$(DRIVER_NAME).so $(OBJS) $(LD_FLAGS)
	strip $(STRIP_FLAGS) $(LIBDIR)/$(DRIVER_NAME).so

$(LIBDIR)/$(DRIVER_NAME).dll: $(OBJS)
	link $(MLFLAGS) /dll /out:$(LIBDIR)/$(DRIVER_NAME).dll \
		$(OBJS) $(ERL_DLL_LIB) $(MLIBS)
	rm $(LIBDIR)/$(DRIVER_NAME).exp $(LIBDIR)/$(DRIVER_NAME).lib

fbx_ops.h: fbx.ops
	perl op_to_h.pl fbx.ops > fbx_ops.h

fbx_ops.hrl: fbx.ops
	perl op_to_hrl.pl fbx.ops > fbx_ops.hrl

$(LIBDIR)/wpc_fbx.beam: fbx_ops.hrl

$(LIBDIR)/%.beam: %.erl
	install -d $(LIBDIR)
	$(ERLC) $(ERLC_FLAGS) $<

wings_fbx.$(OBJ): wings_fbx.cxx fbx_ops.h
	$(CXX) -c $(CFLAGS) wings_fbx.cxx

wings_fbx_driver.$(OBJ): wings_fbx_driver.c fbx_ops.h
	$(CC) -c $(CFLAGS) wings_fbx_driver.c


clean: clean_temp
	-rm -rf fbx *.beam $(LIBDIR)/*.so $(LIBDIR)/*.dll

clean_temp:
	-rm -rf *.o *.obj *.exp *.lib fbx_ops.h fbx_ops.hrl
