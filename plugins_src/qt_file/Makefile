
# wp8_qt_file - Makefile for Unix-like systems
#
# Chris Osgood : 2001/12/14
#

CC = g++
ERL_TOP = /usr/lib/erlang
LD = ld
LDFLAGS = -fPIC -shared -s
LIBS = -lqt

LIBDIR = ../../plugins/qt_file
OBJDIR = .

# Includes
#
INCLUDES = -I. -I$(ERL_TOP)/usr/include -I/usr/include/qt

# Libraries (static)
#

# Compiler options
# 
CFLAGS = $(INCLUDES) -g -O2 -Wall -fPIC

# Object files
#
OBJS = $(OBJDIR)/qt_wings_file_drv.o

DLLS = $(LIBDIR)/qt_wings_file_drv.so

BEAM = $(LIBDIR)/wp8_qt_file.beam
ERLC = erlc
ERLC_FLAGS = -Werror -o $(LIBDIR)

#
# Targets
#

all:	$(DLLS) $(BEAM)

clean: 
	rm -f $(LIBDIR)/*.so
	rm -f $(LIBDIR)/*.o
	rm -f $(LIBDIR)/*.beam

$(LIBDIR)/qt_wings_file_drv.so:qt_wings_file_drv.cpp
	$(CC) $(CFLAGS) -c -o $(OBJDIR)/qt_wings_file_drv.o qt_wings_file_drv.cpp
	install -d $(LIBDIR)
	$(LD) $(LDFLAGS) -o $(LIBDIR)/qt_wings_file_drv.so $(OBJS) $(LIBS)
	rm -f $(OBJDIR)/*.o

$(LIBDIR)/wp8_qt_file.beam: wp8_qt_file.erl
	install -d $(LIBDIR)
	$(ERLC) $(ERLC_FLAGS) wp8_qt_file.erl

