#
#  Makefile --
#
#     Makefile for building the FBX plug-in.
#
#  Copyright (c) 2003-2006 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.7 2006/01/20 14:47:02 giniu Exp $
#
FBXSDK = FBXSDK2005.12a
LIBDIR=../../plugins/fbx
WINGS_INTL=../../intl_tools
WINGS_E3D=../../e3d

OS := $(shell uname -s)

ifneq (,$(findstring CYGWIN,$(OS)))
  FBX_SDK := c:\Program\Autodesk\$(FBXSDK)
  ERL_TOP := $(shell which erl)
  ERL_TOP := $(subst /bin/erl,,$(ERL_TOP))
  ERL_TOP := $(shell cygpath -w $(ERL_TOP))
  DRIVER_NAME := wings_fbx_win32
  DRIVER_FILE := $(DRIVER_NAME).dll
  MLFLAGS = /nologo /subsystem:console /machine:I386
  MLIBS = advapi32.lib $(FBX_SDK)\lib\fbxsdk_mt.lib
  CC = cl -nologo -MT
  CXX = $(CC)
  OBJ = obj
  OS_CFLAGS = -DNDEBUG -DWIN32 -D_CONSOLE -D_MBCS -DK_PLUGIN -DK_FBXSDK
  LIBS = -L$(LIB_DIR) -lfbxsdk
else
  ifeq ($(OS),Linux)
   FBX_SDK := /usr/local/$(FBXSDK)
   DRIVER_NAME := wings_fbx_linux
   OS_LDFLAGS = -shared
   STRIP_FLAGS = --strip-debug 
   LIBS = -L$(LIB_DIR) -lfbxsdk
  else
  # Must be Darwin
   UNIVERSAL_SDK = -isysroot /Developer/SDKs/MacOSX10.4u.sdk
   UNIVERSAL_FLAGS = $(UNIVERSAL_SDK) -arch i386 -arch ppc
   CFLAGS = $(UNIVERSAL_FLAGS)
   FBX_SDK := /Applications/$(FBXSDK)
   DRIVER_NAME := wings_fbx_mac
   OS_LDFLAGS = -bundle -flat_namespace -undefined suppress
   STRIP_FLAGS = -S
   EXTRA_INCLUDES := -I/usr/include/sys
   CXX = /usr/bin/g++-4.0
   LIBS = -L$(LIB_DIR)
  endif
  OS_CFLAGS = -O2 -fPIC
  ERL_TOP := /usr/local/lib/erlang
  DRIVER_FILE := $(DRIVER_NAME).so
  OBJ = o
endif

INCLUDES = -I. -I$(ERL_TOP)/usr/include -I$(FBX_SDK)/include $(EXTRA_INCLUDES)
LIB_DIR = $(FBX_SDK)/lib
CFLAGS = $(INCLUDES) -DWINGS_FBX_DRIVER_NAME='"$(DRIVER_NAME)"' $(OS_CFLAGS) $(UNIVERSAL_FLAGS)

LD_FLAGS = $(OS_LDFLAGS) $(LIBS)

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

ifeq ($(OS),Darwin)
$(LIBDIR)/$(DRIVER_NAME).so: $(OBJS)
	$(CXX) -o ppc_$(DRIVER_NAME).so $(OBJS) $(LD_FLAGS) $(UNIVERSAL_SDK) \
		-arch ppc -lfbxsdk_gcc4
	$(CXX) -o i386_$(DRIVER_NAME).so $(OBJS) $(LD_FLAGS) $(UNIVERSAL_SDK) \
		-arch i386 -lfbxsdk_x86
	strip $(STRIP_FLAGS) ppc_$(DRIVER_NAME).so
	strip $(STRIP_FLAGS) i386_$(DRIVER_NAME).so
	lipo -create ppc_$(DRIVER_NAME).so i386_$(DRIVER_NAME).so \
                -output $(LIBDIR)/$(DRIVER_NAME).so
else
$(LIBDIR)/$(DRIVER_NAME).so: $(OBJS)
	$(CXX) -o $(LIBDIR)/$(DRIVER_NAME).so $(OBJS) $(LD_FLAGS)
	strip $(STRIP_FLAGS) $(LIBDIR)/$(DRIVER_NAME).so
endif

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
