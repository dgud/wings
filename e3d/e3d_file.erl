%%
%%  e3d_file.erl --
%%
%%     Utility functions e3d_file records.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: e3d_file.erl,v 1.2 2004/06/27 11:58:56 bjorng Exp $
%%

-module(e3d_file).

-export([map/2,transform/2,transform_matrix/2]).

-include("e3d.hrl").

map(F, #e3d_file{objs=Objs0}=File) ->
    Objs = lists:map(F, Objs0),
    File#e3d_file{objs=Objs}.

transform(File, Matrix) ->
    map(fun(#e3d_object{obj=Mesh0}=Obj) ->
		Mesh = e3d_mesh:transform(Mesh0, Matrix),
		Obj#e3d_object{obj=Mesh}
	end, File).

transform_matrix(File, Matrix0) ->
    map(fun(#e3d_object{obj=#e3d_mesh{matrix=ObjMatrix}=Mesh0}=Obj) ->
		Matrix = e3d_mat:mul(Matrix0, ObjMatrix),
		Mesh = Mesh0#e3d_mesh{matrix=Matrix},
		Obj#e3d_object{obj=Mesh}
	end, File).


