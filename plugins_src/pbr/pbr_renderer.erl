%%
%%  pbr_renderer.erl
%%
%%     Pbr renderer handling
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-module(pbr_renderer).

-include_lib("wings/src/wings.hrl").
-include("pbr.hrl").
-export([init/2]).

init(St, Attrs) ->
    CL = wings_cl:setup(),
    wings_pb:start(?__(1, "Setup rendering")),
    ForceWG = proplists:get_value(cl_max_workgroup_sz, Attrs, 0),
    S1 = pbr_camera:init(Attrs, #renderer{cl=CL, force_wg=ForceWG}),
    S2 = pbr_scene:init(St, Attrs, S1),
    wings_pb:update(0.99, ?__(2, "Setup film")),
    S3 = pbr_film:init(Attrs, S2),
    Algo = pbr_pathgpu,
    wings_pb:done(),
    spawn(fun() -> 
		  try 
		      register(wings_preview, self()),
		      normal = Algo:start(Attrs, S3)
		  catch _:Err ->
			  io:format("Renderer crashed: ~p~n ~p~n",
				    [Err, erlang:get_stacktrace()])
		  after
 		      wings_cl:stop(CL)
		  end
	  end).

