#
#  Makefile --
#
#     Makefile for building the Wings3D.exe wrapper
#     for starting Wings.
#
#  Copyright (c) 2003 Bjorn Gustavsson
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: Makefile,v 1.1 2003/09/22 05:12:05 bjorng Exp $
#

LIBS = -lkernel32 -luser32 -ladvapi32

Wings3D.exe: wings3d.c wings3d_res.o
	mingw32-gcc -o Wings3D.exe wings3d.c wings3d_res.o $(LIBS)

wings3d_res.o: wings3d.res
	res2coff -i wings3d.res -o wings3d_res.o

#
# The wings3d.res file is included in the source tar file.
#
# To rebuild it, you'll need a resource compiler, such as
# LRC included in the LCC compiler: http://www.cs.virginia.edu/~lcc-win32.
#
# Uncomment the lines below if you have a resource compiler and
# want to rebuild wings3d.res.
#

# RC = lrc

# wings3d.res: wings3d.rc wings.ico doc.ico
# 	$(RC) wings3d.rc
