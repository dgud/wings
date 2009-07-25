%%
%%  e3d.hrl --
%%
%%     Record definition for generic in-memory 3D file format.
%%
%%  Copyright (c) 2001-2008 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

%% 3D vector or location.
-type e3d_vector() :: {float(),float(),float()}.

%% Compact 4x4 matrix representation.
-type e3d_compact_matrix() ::
      {float(),float(),float(),
       float(),float(),float(),
       float(),float(),float(),
       float(),float(),float()}.

%% General 4x4 matrix represention.
-type e3d_matrix() :: 'identity' | e3d_compact_matrix() |
  {float(),float(),float(),float(),
   float(),float(),float(),float(),
   float(),float(),float(),float(),
   float(),float(),float(),float()}.
  
-record(e3d_face,
	{vs=[],				        %List of vertex indices.
	 vc=[],					%Vertex color indices.
	 tx=[],				        %List of texture indices.
	 ns=[],				        %List of normal indices.
	 mat=[],				%Materials for face.
	 sg=1 :: integer(),			%Smooth group for face.
	 vis=-1}).				%Visible edges (as in 3DS).

%% Polygon mesh.
-record(e3d_mesh,
 	{type=triangle :: 'triangle' | 'quad' | 'polygon',
	 vs=[],					%Vertex table (list).
	 vc=[],					%Vertex color table (list).
	 tx=[],					%Texture coordinates (list).
	 ns=[],					%Normal table (list).
 	 fs=[] :: [#e3d_face{}],		%Face table (list of e3d_face).
	 he=[],					%List of chains of hard edges.
	 matrix=identity :: e3d_matrix()	%Local coordinate system.
 	}).

-record(e3d_object,
	{name :: [integer()]|'undefined',	%Name of object (string),
						% or 'undefined' if no name.
	 obj :: #e3d_mesh{},			%Object implementation.
	 mat=[],				%Materials for this object.
	 attr=[]}).				%List of attributes.

-record(e3d_file,
	{objs=[] :: [#e3d_object{}],		%List of objects.
	 mat=[],				%List of materials.
	 creator="", 				%Creator string.
	 dir					%Directory for file.
	}).

