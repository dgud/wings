%%
%%  wpc_pbr.erl
%%
%%     Renderer for wings
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%


-module(wpc_pbr).

-include_lib("wings/src/wings.hrl").
-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").

-export([init/0,menu/2,command/2]).

-define(TAG, wings_pbr).

%% Initialize and check for OpenCL support
init() ->
    case cl:start() of
	ok ->    
	    true;
	{error, {already_started,cl}} ->
	    true;
	{error, _} ->
	    io:format(?__(1, "Warning: OpenCL Support not available rendering disabled~n"),[]),
	    false
    end.

%% Attach to menu 
menu({file,render}, Menu) ->
    case whereis(wings_preview) of
	undefined ->
	    Menu++[{"Wings PBR",?TAG,[option]}];
	Pid when is_pid(Pid) ->
	    Menu++[{"STOP Wings PBR",?TAG}]
    end;
menu(_, Menu) ->
    Menu.

command({file,{render,{?TAG,A}}}, St) ->
    do_export(A, St);
command({file,{render,?TAG}}, St) ->
    do_export([], St);
command(_Spec, _St) ->
    next.

%% Do the export
do_export(Ask, _St) when is_atom(Ask) ->
    RenderFun = fun(Res) -> {file, {render, {?TAG, Res}}} end,
    wpa:dialog(Ask, "Wings Render Options", export_dialog(), RenderFun);

do_export(Attr0, St) when is_list(Attr0) ->
    case whereis(wings_preview) of
	undefined ->
	    Attr = fix_prefs(Attr0),
	    set_prefs(Attr),
	    %% Add Camera, lights to list
	    CameraAttr = [pos_dir_up, fov, hither, yon],
	    CV = wpa:camera_info(CameraAttr),
	    CameraInfo = lists:zip(CameraAttr, CV),
	    All = [{cam,CameraInfo}, 
		   {lights, wpa:lights(St)} | Attr],
	    render(St, All);
	Pid ->
	    io:format("Render STOP requested, patience~n",[]),
	    Pid ! stop,
	    keep
    end.

render(St, Opts) ->
    io:format("~n~n Starting rendering:~n",[]),
    pbr_renderer:init(St, Opts),
    keep.

-define(GET(Key), pget(Key,Opts)). 

export_dialog() ->
    wpa:pref_set_default(?MODULE, default_filetype, ".png"),
    Opts = get_prefs(export_opts()),
    [{hframe,[{label,?__(11,"Sub-division Steps")},
	      {text,?GET(subdivisions),
	       [{key,subdivisions}, {range, {0,5}}]}],
      [{title,?__(10,"Pre-rendering")}]},
     {vframe, [{hframe,
		[{vframe, [{label,?__(21,"Width")},
			   {label,?__(23,"Gamma")}]},
		 {vframe, [{text,?GET(width),[{key,width}]},
			   {text,?GET(gamma),[{key,gamma}]}]},
		 {vframe, [{label,?__(22,"Height")},
			   {label,?__(24,"Filter")}]},
		 {vframe, [{text,?GET(height),[{key,height}]},
			   {menu, [{?__(25,"None"), none},
				   {?__(26,"Preview"), preview},
				   {?__(27,"Gaussian"), gaussian}
				  ],
			    ?GET(film_filter),[{key,film_filter}]}]}]},
	       {hframe, 
		[{vframe, [{label, ?__(28, "Lens diameter (pinhole = 0.0)")},
			   {label, ?__(29, "Preview Refresh (sec)")}]},
		 {vframe, [{text,?GET(aperture),[{key,aperture}]},
			   {text, ?GET(refresh_interval), [{key,refresh_interval}]}]}]}
	      ],
      [{title,?__(20,"Camera and Film")}]},
     {vframe, [{hframe, 
		[{vframe, [{label, ?__(31, "Max Path Depth")},
			   {label, ?__(32, "RR Depth")},
			   {label, ?__(33, "RR Imp Cap")}]},
		 {vframe, [{text, ?GET(max_path_depth), [{key, max_path_depth}]},
			   {text, ?GET(rr_depth), [{key, rr_depth}]},
			   {text, ?GET(rr_imp_cap), [{key, rr_imp_cap}]}
			  ]}]}],
      [{title,?__(30,"Render Engine Options")}]},
     {hframe, [{vframe, 
		[{label, ?__(41, "Sampler Type")},
		 {menu, [{?__(42, "Fastest"), inlined_random},
			 {?__(43, "Random"),  random},
			 %%{?__(44, "Stratified"), stratified},
			 {?__(45, "Metropolis"), metropolis}],
		  ?GET(sampler), [{key, sampler}]},
		 {hframe,
		  [{vframe, [{label, ?__(51, "Samples"), [hook(sampler, stratified)]},
			     {label, ?__(52, "Rate"),    [hook(sampler, metropolis)]},
			     {label, ?__(53, "Reject"),  [hook(sampler, metropolis)]}]},
		   {vframe, [{text, ?GET(stratified_samples), 
			      [{key,stratified_samples}, hook(sampler, stratified)]},
			     {text, ?GET(metropolis_rate), 
			      [{key,metropolis_rate}, hook(sampler, metropolis)]},
			     {text, ?GET(metropolis_reject), 
			      [{key,metropolis_reject}, hook(sampler, metropolis)]}]}]}],
		[{title,?__(50,"Sampler Options")}]},
	       {vframe, 
		[{label, ?__(61, "Filter")},
		 {menu, [{?__(25, "None"), none},
			 {?__(62, "Box"), box},
			 {?__(27, "Gaussian"), gaussian},
			 {?__(63, "Mitchell"), mitchell}],
		  ?GET(filter), [{key, filter}]},
		 {hframe, 
		  [{vframe, [{label, ?__(64, "Dimension"), [hook(filter, {'not', none})]},
			     {label, ?__(65, "Alpha"), [hook(filter, gaussian)]},
			     {label, ?__(66, "Mitchell B"), [hook(filter, mitchell)]},
			     {label, ?__(67, "Mitchell C"), [hook(filter, mitchell)]}]},
		   {vframe, [{text, ?GET(filter_dim), 
			      [{key,filter_dim}, hook(filter, {'not', none})]},
			     {text, ?GET(filter_alpha), 
			      [{key,filter_alpha}, hook(filter, gaussian)]},
			     {text, ?GET(filter_b), 
			      [{key,filter_b}, hook(filter, mitchell)]},
			     {text, ?GET(filter_c), 
			      [{key,filter_c}, hook(filter, mitchell)]}]}],
		  [{title,?__(60,"Filter Options")}]}],
		[{title,?__(40,"Per Pass Engine Options")}]}]}].

hook(Key, {'not', Value}) ->
    Enable = fun(is_disabled, {_Var, _I, Store}) -> 
		     case gb_trees:get(Key, Store) of
			 Value -> true;
			 _ -> false
		     end;
		(_, _) -> void
	     end,
    {hook, Enable};
hook(Key, Value) ->    
    Enable = fun(is_disabled, {_Var, _I, Store}) -> 
		     case gb_trees:get(Key, Store) of
			 Value -> false;
			 _ -> true
		     end;
		(_, _) -> void
	     end,
    {hook, Enable}.

%% Helpers

export_opts() ->
    [%% Scene
     {subdivisions, 0},
     %% Film
     %% {resolution, {512, 256}},
     {width, 512},
     {height, 256},
     {gamma, 2.2},
     {film_filter, none},  %% [none| preview | gaussian ] see: pbr_film
     {refresh_interval, 10},%% Preview refresh rate Secs
     %% Cam extra
     {aperture, 0.0},      %% 
     %% Renderer stuff
     {max_path_depth, 5},
     {rr_depth, 3},
     {rr_imp_cap, 0.125},
     %% Sampler 
     {sampler, inlined_random}, %% [random | stratified | metropolis]
     {stratified_samples, {3,3}},
     {metropolis_rate, 0.4},
     {metropolis_reject, 512},
     %% Pass Filter 
     {filter, none},  %% [none, box, gaussian, mitchell]
     {filter_dim, {1.5, 1.5}},  %% All
     {filter_alpha, 2.0},  %% Gaussian
     {filter_b, 1/3},  %% Mitchell
     {filter_c, 1/3}   %% Mitchell 
    ].

fix_prefs(Attrs0) ->
    Attrs = [fix_pref(Attr) || Attr <- Attrs0],
    [{resolution, {proplists:get_value(width, Attrs),
		   proplists:get_value(height, Attrs)}} | Attrs].

fix_pref({stratified_samples, X}) when is_integer(X) ->
    {stratified_samples, {X,X}};
fix_pref({filter_dim, X}) when is_number(X) ->
    {filter_dim, {X,X}};
fix_pref(X) ->
    X.

pget(Key,Opts) ->    
    case proplists:get_value(Key, Opts) of 
	{X,X} -> X; 
	X -> X 
    end.

set_prefs(Attr) ->
    wpa:scene_pref_set(?MODULE, Attr).

%% get_pref(Key, Def) ->
%%     [{Key,Val}] = get_prefs([{Key,Def}]),
%%     Val.

get_prefs(KeyDefs) when is_list(KeyDefs) ->
    get_prefs_1(KeyDefs, make_ref()).

get_prefs_1([{Key,Def}|KeyDefs], Undefined) ->
    Value = case wpa:scene_pref_get(?MODULE, Key, Undefined) of
	      Undefined ->
		  wpa:pref_get(?MODULE, Key, Def);
	      Val ->
		  Val
	  end,
    [{Key,Value}|get_prefs_1(KeyDefs, Undefined)];
get_prefs_1([], _Undefined) ->
    [].
