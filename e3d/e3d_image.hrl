%%
%%  e3d_image.hrl --
%%
%%     Handle images (2D) and different file formats.
%%
%%  Copyright (c) 2001 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d_image.hrl,v 1.4 2003/01/21 20:15:25 bjorng Exp $
%%

-record(e3d_image,           %% Currently supported formats:
	{type = r8g8b8,      %% [g8 (gray8) or a8 (alpha8) 
	                     %% r8g8b8 or r8g8b8a8 or b8g8r8 or b8g8r8a8]
	 bytes_pp = 3,       %% bytes per pixel
	 alignment = 1,      %% A = 1|2|4 Next row starts direct|even 2|even 4
	 order = lower_left, %% First pixel is in:
	                     %% lower_left,lower_right,upper_left,upper_right]
	 width = 0,          %% in pixels
	 height = 0,         %% in pixels
	 image,              %% binary
	 filename=none,	     %% Filename or none
	 name=[]	     %% Name of image
	}).

