%%
%%  e3d.hrl --
%%
%%     Record definition for generic in-memory 3D file format.
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d.hrl,v 1.9 2004/10/12 17:44:34 bjorng Exp $
%%

-record(e3d_file,
	{objs=[],				%List of objects.
	 mat=[],				%List of materials.
	 creator="", 				%Creator string.
	 dir					%Directory for file.
	}).

-record(e3d_object,
	{name,					%Name of object (string),
						% or 'undefined' if no name.
	 obj,					%Object implementation.
	 mat=[],				%Materials for this object.
	 attr=[]}).				%List of attributes.

%% Polygon mesh.
-record(e3d_mesh,
 	{type=triangle,				%'triangle' or 'polygon'.
	 vs=[],					%Vertex table (list).
	 vc=[],					%Vertex color table (list).
	 tx=[],					%Texture coordinates (list).
	 ns=[],					%Normal table (list).
 	 fs=[],					%Face table (list of e3d_face).
	 he=[],					%List of chains of hard edges.
	 matrix=identity			%Local coordinate system.
 	}).

-record(e3d_face,
	{vs=[],					%List of vertex indices.
	 vc=[],					%Vertex color indicies.
	 tx=[],					%List of texture indices.
	 ns=[],					%List of normal indicies.
	 mat=[],				%Materials for face.
	 vis=-1}).				%Visible edges (as in 3DS).

