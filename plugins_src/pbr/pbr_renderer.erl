%%
%%  pbr_renderer.erl
%%
%%     Pbr renderer handling
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-module(pbr_renderer).

-include("pbr.hrl").

-export([init/2]).

init(St, Attrs) ->
    CL = wings_cl:setup(),
    ForceWG = proplists:get_value(cl_max_workgroup_sz, Attrs, 0),
    S1 = pbr_camera:init(Attrs, #renderer{cl=CL, force_wg=ForceWG}),
    S2 = pbr_scene:init(St, Attrs, S1),
    S3 = pbr_film:init(Attrs, S2),
    Algo = pbr_pathgpu,
    spawn(fun() -> 
		  try 
		      register(wings_preview, self()),
		      normal = Algo:start(Attrs, S3) 
		  catch _:Err ->
			  io:format("Renderer crashed: ~p~n ~p~n",
				    [Err, erlang:get_stacktrace()])
		  end
	  end).

