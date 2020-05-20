%%
%%  e3d.hrl --
%%
%%     Record definition for generic in-memory 3D file format.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

%% Types for e3d_bv
-define(E3D_INFINITY, 3.402823e+38).  %% 32 bits float max
-type e3d_bbox() :: {e3d_vec:point(), e3d_vec:point()}.
-type e3d_bsphere() :: {e3d_vec:point(), number()}.
-type e3d_bv() :: e3d_bbox() | e3d_bsphere().

%% Types for transform
-record(e3d_transf,
	{mat = e3d_mat:identity() :: e3d_mat:matrix(),
	 inv = e3d_mat:identity() :: e3d_mat:matrix()}).

-type e3d_transform() :: #e3d_transf{}.


-record(ray,
	{o::e3d_vec:point(),
         d::e3d_vec:vector(),
	 n::float(),                            % Near, far (or MinT MaxT)
         f::float(),
         bfc=true::boolean()                    % Backface culling?
        }).

-type e3d_ray() :: #ray{}.

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
	 matrix=identity :: e3d_mat:matrix()	%Local coordinate system.
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
