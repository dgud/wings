%%
%%  Copyright (c) 2004 Rolf Stenholm
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_rspt_re.erl,v 1.1 2004/08/25 05:33:06 bjorng Exp $
%%

%%
%%Original rendering system
%%
%% TAINTED_MODULE


%%
%%Plugin and system ?
%%For rendering good high quality images ..
%%
%%
%%
%%Specs: preliminar
%%Required: Need some timers and clockers for this one to report stats ...
%%
%%
%%Ambiance factor could possible be calculated using radiosity ... hmmm
%% requires a lot of memory and cpu, later ....
%%
%%For eacg segment of image do
%% Render scene with ambient (depth on), 
%%   passes +ambient +reflection +refraction
%% For each light do (all operations additive)
%%  for each polygon not transparent do
%%    render stencil shadow on polygon
%%    enable stencil clipping
%%    enable addition of source and target (yes, multiple light sources)
%%    enable bump mapping
%%    passes +diffuse +refraction +reflection +specular 
%%    disable bump mapping ?
%%    passes +specular iridiscence ? (maybe)
%% For each light do 
%%   for each transparent polygon
%%    render stencil shadow on polygon
%%    enable stencil clipping
%%    enable addition of source and target (yes, multiple light sources)
%%    enable bump mapping
%%    passes +diffuse +refraction +reflection +specular 
%%    disable bump mapping ?
%%    passes +specular iridiscence ? (maybe)
%%   
%%
%%
%%Unsolved Problems: 
%%   bump maps implemntation desgin application ??? (no problem at all)
%%   transparent surfaces are not handled properly ??? (still an issue but can be managed)
%%   introduction of spherical application of textures orientation and more ..
%%
%%
%%
%%A lot has changed from the above or is unimplemented
%%
%%Faces are rendered with the back side front, wich is right hand order of polygons
%% .. unlikely to make any difference 

%%
%%For now put into menu and so on
%%
%%


-module(wpc_rspt_re).
-define(NEED_ESDL,1).
-define(NEED_OPENGL,1).
-include_lib("wings.hrl").
-include_lib("e3d.hrl").
-include_lib("e3d_image.hrl").

-export([init/0,menu/2,command/2,get_pref/2,help/0]).
-export([get_tilesize/0]).
-define(debugP(X,W),ok).
-define(debugR(X,W),ok).
%-define(debugR(X,W),io:format(X,W)).
-define(debugM(X,W),ok).
%-define(debugM(X,W),io:format(X,W)).
-define(debug2(X,W),ok).
%-define(debugN(X,W),io:format(X,W)).
-define(debugN(X,W),ok).

%% must be the same as in wpc_rspt_re
%-define(CULL_FRONT,?GL_BACK).
%-define(CULL_BACK,?GL_FRONT).
-define(CULL_FRONT,?GL_FRONT).
-define(CULL_BACK,?GL_BACK).


%% link into app
init()->
	%%io:format("Loading wpc_rspt_re ... ~n",[]),
	true.

menu({file,render},Menu)->
	[{"RSPT render",rspt_render,[option]}] ++ Menu;
menu(rspt_help,Menu)->
	Menu ++ [{"Rendering with RSPT",{wpc_rspt_re,help}}];
menu(X, Menu)->
	%%io:format("Render recieved ~p menu cmd~n",[X]),
	Menu.

%% should have answer to rspt_render_help as well
command({file,{render,{rspt_render,Ask}}},St)->
	do_render(Ask,St);
command(_,_)-> next.


%%%%%% documentation of this plugin
help()->
	wpc_rspt_wm:help_window(wpc_rspt_re_help_window,"Rendering images",
		["Select file -> render -> RSPT render from the wings menu to render images with this plugin",
		 "This renderer has a large number of features including shadows, per pixel lightning, mirror surfaces"
		 ", multisampling and a plugin system. "
		 "To select render options select file -> render -> RSPT render, and click the box. "
		 "You will see several popup menus and fields. See below for a short description.",
		
		 "Destination menu",
		 "Render to screen: "
		 "Render result into an image",
		 "Render to file: "
		 "Store result in specified file",
 		 "",

		 "Paint buffer menu",
		 "Render AUX0: "
		 "Paint using auxilary buffer on graphics card. Best option if your graphics card supports it.",
		 "Render visibly: "
		 "Paint using the screen.",
		 "Render invisibly: "
		 "Render using the screen but invisibly.",
		 "",
		 "Quality menu",
		 "High quality: "
		 "Use multisampling and other techniques to improve render quality.",
		 "Low quality: "
		 "Render as fast as possible",
		 "",
		 "Tile size menu",
		 "Always use the max setting unless you experience problems.",
		"",
		"Temporary result buffer",
		"Texture as temp storage: "
		"Best and fastest option if your graphics card supports this",
		"Accumulation buffer as temp storage: "
		"An alternative option that may be better and faster on some graphics cards",
		"Anti aliasing",
		"True anti aliasing, improves render quality. Can be used in addition to any multi sampling", 
		"Width: "
		"Image width",
		"Height: "
		"Image height",
		"Subdivisions: "
		"Amount of smooth operations to apply to all objects in scene." 
		
		],
		[]).

%%%
%%% Needs to be fixed to use previous selected values
%%%
dialog(rspt_render)->
	[{label,"Destination"},
	 {menu,[{"Render to Image window",to_window},
		{"Render to file",to_file}],
		get_pref(destination,to_window),[{key,destination}]},
	 {label,"Paint buffer"},
	 {menu,[{"Render visible",?GL_FRONT_AND_BACK},
		{"Render invisible",?GL_BACK},
		{"Render AUX0",?GL_AUX0}],
		get_pref(selectbuffer,?GL_AUX0),[{key,selectbuffer}]},
	{label,"Quality"},
	{menu,[ {"High quality",high},
		{"Low quality",low}],
		get_pref(quality,high),[{key,quality}]},
	{label,"Tile size"},
	%% should build from screen size
	{menu,%%[ {"Tiles 256px" ,256 },
		%{"Tiles 512px" ,512 },
		%{"Tiles 1024px",1024},
		%{"Tiles 2048px",2048},
		%{"Tiles 4096px",4096},
		%{"Tiles Max",max}],
		get_tilevals(256) ++ [{"Tile size MAX pixels",max}],
		get_pref(tile_size,max),[{key,tile_size}]},
	{label,"Temporary result buffer"},
	{menu,[{"Accumulation buffer as temp storage",sto_accum},
		{"Texture as temp storage",sto_texture}],
		get_pref(sto_buffer,sto_texture),[{key,sto_buffer}]},
	{label,"Anti aliasing"},
	{menu,[{"No AA",1},
	       {"AA x2",2},
	       {"AA x4",4},
	       {"AA x8",8},
	       {"AA x16",16},
	       {"AA x32",32},
		{"AA x64",64},
		{"AA x128",128},
		{"AA x256",256},
		{"AA x512",512}],
	        get_pref(anti_aliasing,1),[{key,anti_aliasing}]},
	{hframe,[
  	 {hframe,[{label,"Width"},{text,get_pref(image_width,640),[{key,image_width}]}]},	
	 {hframe,[{label,"Height"},{text,get_pref(image_height,480),[{key,image_height}]}]}
	]},
	{hframe,[{label,"Subdivisions"},{text,get_pref(subdivisions,0),[{key,subdivisions}]}]},

	{hframe,[{label,"Shadow detail"},{text,get_pref(shadow_detail,1),[{key,shadow_detail}]}]},
	{label,"Shadow polygon offset"},
	{menu,[{"No polygon offset",no_off},
	       {"Polygon offset",offset}],
		get_pref(shadow_polygon_offset,no_off),[{key,shadow_polygon_offset}]}
	
	].



do_render(Ask,_St) when is_atom(Ask) ->
	wpa:dialog(Ask,"Render options",
		dialog(rspt_render),
		fun(RES) ->
			{file,{render,{rspt_render,RES}}}
		end);
do_render(Attr,St)->
	%%io:format("do_render ~p ~n",[Attr]),
	set_pref(Attr),
	wings_io:hourglass(),
	%%% should I switch here, no probably in function below
	%%renderImage(fun(ExtObj)-> light_render_all(St,ExtObj), ok end),
	St1 = scene_subdivide(St),
	%%
	%%Should patch compile in such a fashion that compile is done only once ?? ...
	%%
	renderFatImage(fun(ExtObj)-> light_render_all(St1,ExtObj), ok end,St1),
	St.

%% the misc stuff

get_pref(Key,Def)->
	%%wpa:pref_get(?MODULE,Key,Def).
	wpc_rspt_ps:get_pref(Key,Def).
	
set_pref(KeyVals) ->
	%%wpa:pref_set(?MODULE,KeyVals).
	wpc_rspt_ps:set_pref(KeyVals).

%%%
%%%frame work for render image ...
%%%



renderFatImage(ImageF,St)->
	%%% fetch image size
	Width     = clamp(get_pref(image_width,640) ,1,999999999),
	Height    = clamp(get_pref(image_height,480),1,999999999),
	%%% fetch tile size, should clamp to screen ...
	TileSize  = get_tilesize(),
	ScantileX = trunc(Width  / (TileSize+0.01))+1,
	ScantileY = trunc(Height / (TileSize+0.01))+1,
	ScanX     = TileSize/Width,
	ScanY     = TileSize/Height,
	Entries   =  [ {X*ScanX,Y*ScanY,(X+1)*ScanX,(Y+1)*ScanY} 
			|| Y <- lists:seq(0,ScantileY-1), X <- lists:seq(0,ScantileX-1)],
	%% build scene here and pass it to everyone else
	RenderReq = render_req_build(St),
	%% render all with as proper data
	Images = lists:map(fun({X,Y,X1,Y1})->
			renderTile(X,Y,X1,Y1,TileSize,Width/Height,ImageF,RenderReq)	
		  end,Entries),

	%%io:format("~nTiles ~p  Width=~p Height=~p ~n~n",[Entries,Width,Height]),

	%% build image
	Data = scissor_tile(glue_tiles(Images,ScantileX),Width,Height,TileSize),
	WingImg = #e3d_image{bytes_pp=3,order=lower_left,width=Width,height=Height,image=list_to_binary(lists:flatten(Data))},

	%% this part should be done somewhere else
	%%ID  = wings_image:new("RSPT Render",WingImg),
	%%wings_image:window(ID),
	outputFatImage(WingImg,St),

	%%
	%% free any compiled data
	RenderReq(delete),
	io:format("GL state ~p~n",[glu:errorString(gl:getError())]),
	%%io:format("Exiting fat render !!!!!~n",[]),
	gl:clear(?GL_COLOR_BUFFER_BIT), %% remove stuff
	wings_wm:dirty().                 


outputFatImage(WingImg,St)->
	case get_pref(destination,to_window) of
		to_window ->
			ID  = wings_image:new("RSPT Render",WingImg),
			wings_image:window(ID),
			ok;
		to_file ->
			Props = [{ext,".tga"},{ext_desc,"Targa file"}],
			case wpa:export_filename(Props,St) of
				aborted ->
					%% is this possible to happen ?? 
					io:format("Unable to write to file~n",[]),
					ok;
				File    ->
					io:format("Writing to file ~p ... ~n",[File]),
					e3d_image:save(WingImg,File),
					ok
			end
	end.

renderTile(X,Y,X1,Y1,TileSize,Aspect,ImageF,RenderReq)->
	%%io:format("Render ~p ~p ~p ~p ~n",[X,Y,X1,Y1]),
	[MTex] = gl:genTextures(1),
	Disp = fun(Param)->
			case Param of
				{setup_camera_view,AAF} ->
					[W_Fov,W_Hither,W_Yon,W_dist,W_elev,W_azimuth,W_aim] 
						= wpa:camera_info([fov,hither,yon,distance_to_aim,elevation,azimuth,aim]),
					gl:viewport(0,0,TileSize,TileSize), % since this may be modified
					gl:matrixMode(?GL_PROJECTION),
					gl:loadIdentity(),
					AAF(),
					F = W_Hither/2,
					gl:frustum((-1+2*X)*F*Aspect,(-1+2*X1)*F*Aspect,(-1+2*Y)*F,(-1+2*Y1)*F,2*F,W_Yon),
					gl:matrixMode(?GL_MODELVIEW),
					gl:loadIdentity(),	
					%% do AA transforms
					wings_view:model_transformations(false), % should skip all in the future and do light by hand
					ok;
				setup_camera_view ->
					%%{_,_,W,H} = wings_wm:viewport(),
					[W_Fov,W_Hither,W_Yon,W_dist,W_elev,W_azimuth,W_aim] 
						= wpa:camera_info([fov,hither,yon,distance_to_aim,elevation,azimuth,aim]),
					gl:viewport(0,0,TileSize,TileSize), % since this may be modified
					gl:matrixMode(?GL_PROJECTION),
					gl:loadIdentity(),
					%%F = 0.05,
					%% was 10000 as last value changed to 1000 ( was 1000*F)
					F = W_Hither/2,
					gl:frustum((-1+2*X)*F*Aspect,(-1+2*X1)*F*Aspect,(-1+2*Y)*F,(-1+2*Y1)*F,2*F,W_Yon),
					%%glu:perspective(W_Fov,1.0,W_Hither,W_Yon),
					gl:matrixMode(?GL_MODELVIEW),
					gl:loadIdentity(),	
					% check correctness ..
					wings_view:model_transformations(false), % should skip all in the future and do light by hand
					ok;
				scene_object ->
					RenderReq(scene_object);
				%% initalize storage
				init_storage ->
					img_init_storage(MTex);
				%% blend image with current stored items
				{add_image,NLight} ->
					img_add_image(MTex,NLight);
				{get_image,NLights} ->
					img_get_image(MTex,NLights);

				%% chained toghether with renderReq, (inheritance to renderReq)
				ParentReq ->
					case RenderReq(ParentReq) of
						refuse ->	
							io:format("ExtObj request ~p ~n",[Param]),
							%% cowardly refuse request
							refuse;
						ReqRes ->
							ReqRes
					end
			end	
		end,
	gl:pushAttrib(?GL_ALL_ATTRIB_BITS),
	gl:pushClientAttrib(?GL_CLIENT_ALL_ATTRIB_BITS),
	ImageF(Disp),
	gl:popClientAttrib(),
	gl:popAttrib(),
	gl:deleteTextures(1,{MTex}),
	captureTile().	


get_tilevals(Start)->
	{_,_,W,H} = wings_wm:viewport(),
	case {W > H,Start < W,Start < H } of
		{true,true,_} ->
			[{"Tile size " ++ integer_to_list(Start) ++ " pixels",Start} | get_tilevals(Start*2)];
		{false,_,true}->
			[{"Tiles size " ++ integer_to_list(Start) ++ " pixels",Start} | get_tilevals(Start*2)];
		_             ->
			[]
			%%[{"Tile size 4096 (exp)",4096}]
	end.

get_tilesize()->
	case get_pref(tile_size,max) of
		
		max ->
			%% the max val of viewport (for now)
			[Max|_] = lists:reverse(lists:sort([V || {_,V} <- get_tilevals(64)])),
			Max;
		Val ->
			Val
	end.



%%%
%%% Capture
%%% Need some capture stuff here
%%%

capture(N, Type)->
	gl:pixelStorei(?GL_PACK_ALIGNMENT,1),
	%% fix this now as
	%%gl:readBuffer(?GL_FRONT),
	gl:readBuffer(get_pref(selectbuffer,?GL_AUX0)),
	{X,Y,W,H} = wings_wm:viewport(),
	Length = N*W*H,
	Mem = sdl_util:malloc(Length,?GL_UNSIGNED_BYTE),
	gl:readPixels(X,Y,W,H,Type,?GL_UNSIGNED_BYTE,Mem),
	Data = sdl_util:readBin(Mem,Length),
	sdl_util:free(Mem),
	#e3d_image{bytes_pp=N,order=lower_left,width=W,height=H,image=Data}.


%%
%%Same as above, only for tiles
%%
captureTile()->
	gl:pixelStorei(?GL_PACK_ALIGNMENT,1),
	%% fix this now as
	%%gl:readBuffer(?GL_FRONT),
	gl:readBuffer(get_pref(selectbuffer,?GL_AUX0)),
	TileSize = get_tilesize(),
	Length = 3*TileSize*TileSize,
	Mem = sdl_util:malloc(Length,?GL_UNSIGNED_BYTE),
	gl:readPixels(0,0,TileSize,TileSize,?GL_RGB,?GL_UNSIGNED_BYTE,Mem),
	Data = sdl_util:readBin(Mem,Length),
	sdl_util:free(Mem),
	Data.

%%
%%Clamp values to range
%%
clamp(V,Floor,Ceiling)->
	case { V<Floor , V > Ceiling }  of
		{true,_} ->
			Floor;
		{_,true} ->
			Ceiling;
		{_,_}    ->
			V
	end.

%%%
%%%Both of these will build on gb_trees ability to cope with arbitary sized trees,
%%%balance and relative small memory footprint. should not take more than 10x of image
%%%memory.
%%%The method should however be stable in the sense that every new gb_tree will only
%%%be ln(Tree) of orginal system in memory footprint.



%%%
%%%Will operate on a gb_tree and filter all values that go beyond the viewpoint
%%%should execute without producing additional fragments.
%%%
%%%Operates on scanline fragments of original images
%%%
scissor_tile(DataT,Width,Height,TileSize)->
	TileWidth = trunc(Width / (TileSize+0.01))+1, %% by definition
	scissor_tile(0,0,DataT,TileWidth,Height,TileSize,Width).

%% still correct termination Y is in Y coord X in tiles
scissor_tile(_,Height,_,_,Height,_,_)->
	[];
scissor_tile(Width,Y,DataT,Width,Height,TileSize,RealWidth)->
	scissor_tile(0,Y+1,DataT,Width,Height,TileSize,RealWidth);
scissor_tile(X,Y,DataT,Width,Height,TileSize,RealWidth)->
	Bin = gb_trees:get({X,Y},DataT),
	NBin = case X+1 of
			Width ->
				%% cut data to proper size
				CutWidth = 24*(RealWidth - X*TileSize),
				<< Ibin:CutWidth,Rest/binary >> = Bin,
				<< Ibin:CutWidth >>;
			_     ->
				Bin
		end,
	[NBin |scissor_tile(X+1,Y,DataT,Width,Height,TileSize,RealWidth)].

%%%
%%% glue function to glue a tile of elements into a single tile ...
%%% done in erlang space to avoid gpu limitations
%%%
%%% fun: integer>0 * interger>0 * list of data lists
%%% out: data
%%%
glue_tiles(Tiles,Scantile)->
	TileSize = get_tilesize(), %%clamp(get_pref(tile_image_size,512),1,9999999999999),
	glue_tiles(0,0,TileSize,Scantile,Tiles,gb_trees:empty()).
%%
%%glue tiles
%%
glue_tiles(_,_,_,_,[],DataT)->
	DataT;
glue_tiles(TileScan,Y,TileSize,TileScan,Tiles,DataT)->
	glue_tiles(0,Y+1,TileSize,TileScan,Tiles,DataT);
glue_tiles(X,Y,TileSize,TileScan,[Tile|Tail],DataT)->
	%%io:format("glue tile into tree ~p ~p~n",[X,Y]),
	glue_tiles(X+1,Y,TileSize,TileScan,Tail,glue_tile_into_tree(X,Y,TileSize,Tile,DataT)).

%%
%%Insert fragments into tree at correct position. (X,Y,Scanline is in bytes)
%%
glue_tile_into_tree(Tx,Ty,TileSize,TileD,DataT)->
	glue_tile_into_tree(Tx,Ty,TileSize,0,TileD,DataT).

%% end insert into tree
glue_tile_into_tree(_,_,TileSize,TileSize,_,DataT)->
	DataT;
glue_tile_into_tree(Tx,Ty,TileSize,Y,Frags,DataT)->
	%% fetch binary scanline
	PreSize = Y*TileSize*24,
	LineSize = TileSize*24,
	%%io:format("<<<<<<<< Fetching binaries ~p ~p ~p ~n",[Y,PreSize,LineSize]),
	<<	Prev:PreSize,
		Binline:LineSize,
		Tail/binary>> = Frags,
	%%io:format("~n Done fetching binaries >>>>>>>>>~n",[]),
	%% build key
	Key = {Tx,Ty*TileSize + Y}, %% Tx is current tile coordinates
	glue_tile_into_tree(Tx,Ty,TileSize,Y+1,Frags,gb_trees:insert(Key,<<Binline:LineSize>>,DataT)).	

%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% 
%%%%%  Rendering methods below ...
%%%%%

%% should test rendering objects in view space and applying depth buffers and so on
test_render(St)->
	gl:drawBuffer(?GL_FRONT),
	gl:clearColor(0,0,0,1),
	gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
	ok.

%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% camera setup stuff
%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%

%
%Should 
%
setup_camera_view(ExtObj) when is_atom(ExtObj) ->
	{_,_,W,H} = wings_wm:viewport(),
	[W_Fov,W_Hither,W_Yon,W_dist,W_elev,W_azimuth,W_aim] 
		= wpa:camera_info([fov,hither,yon,distance_to_aim,elevation,azimuth,aim]),
	gl:viewport(0,0,W,H), % since this may be modified
	gl:matrixMode(?GL_PROJECTION),
	gl:loadIdentity(),
	glu:perspective(W_Fov,W/H,W_Hither,W_Yon),
	gl:matrixMode(?GL_MODELVIEW),
	gl:loadIdentity(),	
	wings_view:model_transformations(true), % should skip all in the future and do light by hand
	ok;
setup_camera_view(ExtObj)->
	%%
	%%Fetch precomputed values for camera
	%%
	%%io:format("Using ExtObj as camera setup fun ~n",[]),
	ExtObj(setup_camera_view).
	








%%%%%%%%%%%
%%%%%%%%%%% Render from light point towards direction (spot light with self shadowing ...)
%%%%%%%%%%% 

%%%%%%
%%%%%% Version 09 of light, info from wings_light.erl
%%%%%%
-record(light,
	{type,
	diffuse={1.0,1.0,1.0,1.0},
	ambient={0.0,0.0,0.0,1.0},
	specular={1.0,1.0,1.0,1.0},
	aim,                      % aim spot
	lin_att,
	quad_att,
	spot_angle,
	spot_exp,
	prop=[]                    % extra properties whoo-hoo what does wings handle here ???
	}).


%%
%%Set hints and multisampling ..
%%
quality_setup()->
	case get_pref(quality,high) of
		high ->
			gl:hint(?GL_PERSPECTIVE_CORRECTION_HINT,?GL_NICEST),
			gl:hint(?GL_POINT_SMOOTH_HINT,?GL_NICEST),
			gl:hint(?GL_LINE_SMOOTH_HINT,?GL_NICEST),
			gl:hint(?GL_POLYGON_SMOOTH_HINT,?GL_NICEST),
			gl:enable(?GL_MULTISAMPLE);	
		low ->
			gl:hint(?GL_PERSPECTIVE_CORRECTION_HINT,?GL_FASTEST),
			gl:hint(?GL_POINT_SMOOTH_HINT,?GL_FASTEST),
			gl:hint(?GL_LINE_SMOOTH_HINT,?GL_FASTEST),
			gl:hint(?GL_POLYGON_SMOOTH_HINT,?GL_FASTEST),
			gl:disable(?GL_MULTISAMPLE)	
	end.


%%
%%
%%mirrors and other stuff should have special treatment
%%
%%ExtObj is an object for passing additional data
%%
%%light_render_all_old(#st{shapes=Sh}=St,ExtObj)->
%%%
%
%	% should change sooner or later, only front...
%	gl:drawBuffer(get_pref(selectbuffer,?GL_AUX0)),
%	quality_setup(),%
%
%	%%io:format("Current buffer for writing is ~p",[get_pref(selectbuffer,?GL_FRONT_AND_BACK)]),
%	%%gl:clearColor(0,0,0,1),
%	%%gl:clearAccum(0,0,0,0),
%	gl:clearColor(0,0,0,1),
%	gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT bor ?GL_ACCUM_BUFFER_BIT),
%
%	%% may need to set current frame buffer
%	ExtObj(init_storage),
%
%	gl:enable(?GL_DEPTH_TEST),
%	gl:polygonMode(?GL_FRONT_AND_BACK,?GL_FILL),
%	% anti alias polygon, interfers use OpenGL 1.4 super sampling instead
%	gl:disable(?GL_POLYGON_SMOOTH),
%	gl:shadeModel(?GL_SMOOTH),
%	gl:enable(?GL_LIGHTING),
%	gl:enable(?GL_CULL_FACE),
%	gl:cullFace(?GL_FRONT),
%	gl:lightModeli(?GL_LIGHT_MODEL_TWO_SIDE,?GL_FALSE),
%	
%	wpc_rspt_ma:texture_compile(St), %% texture bind
%
%	gl:depthFunc(?GL_LEQUAL),
%	
%	% required because of wings ...
%	gl:polygonOffset(?GL_POLYGON_OFFSET_POINT,0),
%	gl:polygonOffset(?GL_POLYGON_OFFSET_LINE,0),
%	gl:polygonOffset(?GL_POLYGON_OFFSET_FILL,0),
%
%	Scene = ExtObj(scene_object),
%	light_ambient(St,Scene,ExtObj),
%
	%%
	%%Require even more passes for diffuse,refraction,specular,mirror separation
	%% diffuse and specular passes can use the same depth data, should probably render both
	%% in two passes ??.
	%% As long as additive lightning is used all operations may be reordered ..
	%% Should probably do diffuse and specular in the same stage ... should make
	%% plugins easier to write ...
	%%
	%% what order to render lights in ??, ignoring for now  
	%%
%	NLights = lists:foldl(fun({N,X},NumberLights)->
%				case ?IS_LIGHT(X) of
%					true ->
%						light_render(X,St,Scene,NumberLights+1,ExtObj),
%						NumberLights+1;
%					false ->
%						NumberLights
%				end
%			end,1,gb_trees:to_list(Sh)),
%
	%% fetch the accum buffer
	%% switching in capture mode, no need for this
	%%gl:drawBuffer(?GL_FRONT),
	%%gl:accum(?GL_RETURN,NLights),
%	ExtObj({get_image,NLights}),

%	gl:flush(),
%
	%% not done here
	%%gl:deleteLists(Scene,7),
%	ok.


%%
%%
%%mirrors and other stuff should have special treatment
%%
%%ExtObj is an object for passing additional data
%%
light_render_all(#st{shapes=Sh}=St,ExtObj)->


	gl:bindTexture(?GL_TEXTURE_2D,0), %% make sure we are entering correctly
	
	% should change sooner or later, only front...
	gl:drawBuffer(get_pref(selectbuffer,?GL_AUX0)),
	quality_setup(),

	%%io:format("Current buffer for writing is ~p",[get_pref(selectbuffer,?GL_FRONT_AND_BACK)]),
	%%gl:clearColor(0,0,0,1),
	gl:clearAccum(0,0,0,0),
	gl:clearColor(0,0,0,1),
	gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT bor ?GL_ACCUM_BUFFER_BIT),
	%%gl:clear(?GL_ACCUM_BUFFER_BIT),

	%% may need to set current frame buffer
	ExtObj(init_storage),

	gl:enable(?GL_DEPTH_TEST),
	gl:polygonMode(?GL_FRONT_AND_BACK,?GL_FILL),
	% anti alias polygon, interfers use OpenGL 1.4 super sampling instead
	gl:disable(?GL_POLYGON_SMOOTH),
	gl:shadeModel(?GL_SMOOTH),
	gl:enable(?GL_LIGHTING),
	gl:enable(?GL_CULL_FACE),
	gl:cullFace(?CULL_FRONT),
	%%gl:cullFace(?GL_FRONT),
	gl:lightModeli(?GL_LIGHT_MODEL_TWO_SIDE,?GL_FALSE),
	
	wpc_rspt_ma:texture_compile(St), %% texture bind

	gl:depthFunc(?GL_LEQUAL),
	
	% required because of wings ...
	%gl:polygonOffset(?GL_POLYGON_OFFSET_POINT,0),
	%gl:polygonOffset(?GL_POLYGON_OFFSET_LINE,0),
	%gl:polygonOffset(?GL_POLYGON_OFFSET_FILL,0),
	gl:disable(?GL_POLYGON_OFFSET_POINT),
	gl:disable(?GL_POLYGON_OFFSET_LINE),
	gl:disable(?GL_POLYGON_OFFSET_FILL),

	Scene = ExtObj(scene_object),

	NLights = lists:foldl(fun(V,Sadds)->
				%% may still need adjustments
				SF = 2.0/get_tilesize(),
				XF = SF*(random:uniform()-0.5),
				YF = SF*(random:uniform()-0.5),
				ZF = SF*(random:uniform()-0.5),

				%%io:format("Chage vals are  ~p ~p ~p ~n",[XF,YF,ZF]),

				AAF = fun()->
					gl:translatef(XF,YF,ZF)
					end,
			
	
				XXobj = fun(X) ->
					
						case X of
							setup_camera_view ->
								ExtObj({setup_camera_view,AAF});
							_ ->
								ExtObj(X)
						end
					end,
							
				Qant = light_pass_all(St,XXobj,Scene,Sadds),
				%%io:format("Number of passes is ~p ~n",[Qant]),
				Qant
			      end,0,lists:seq(1,get_pref(anti_aliasing,1))),

	%% fetch the accum buffer
	%% switching in capture mode, no need for this
	ExtObj({get_image,NLights}),

	gl:flush(),
	ok.

light_pass_all(#st{shapes=Sh}=St,ExtObj,Scene,Sadds)->

	gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT bor ?GL_STENCIL_BUFFER_BIT),
	Sadds0 = light_ambient(St,Scene,ExtObj,Sadds),

	%%
	%%Require even more passes for diffuse,refraction,specular,mirror separation
	%% diffuse and specular passes can use the same depth data, should probably render both
	%% in two passes ??.
	%% As long as additive lightning is used all operations may be reordered ..
	%% Should probably do diffuse and specular in the same stage ... should make
	%% plugins easier to write ...
	%%
	%% what order to render lights in ??, ignoring for now  
	%%
	NLights = lists:foldl(fun({N,X},NumberLights)->
				case ?IS_LIGHT(X) of
					true ->
						light_render(X,St,Scene,NumberLights+1,ExtObj),
						NumberLights+1;
					false ->
						NumberLights
				end
			end,Sadds0,gb_trees:to_list(Sh)).

%
%Eliminate all light
%
light_alloff()->
	gl:disable(?GL_LIGHT0),
	gl:disable(?GL_LIGHT1),
	gl:disable(?GL_LIGHT2),
	gl:disable(?GL_LIGHT3),
	gl:disable(?GL_LIGHT4),
	gl:disable(?GL_LIGHT5),
	gl:disable(?GL_LIGHT6),
	gl:disable(?GL_LIGHT7),
	gl:disable(?GL_TEXTURE_2D),
	ok.


light_ambient(St,Scene,ExtObj)->
	light_ambient(St,Scene,ExtObj,1).

%%
%%render ambient light ... no shadows
%%
light_ambient(#st{shapes=Sh,mat=Mat}=St,Scene,ExtObj,Sadds)->

	io:format(">>> Calling ambient light <<<~n",[]),	
	% render everything from camera
	setup_camera_view(ExtObj),
	light_alloff(),
	%% required now for scene rendering
	%%ExtObj(mat_full),
	%% will be done later
	gl:disable(?GL_BLEND),
	Ambient = lists:foldl(fun({N,X},Acc)->
			case X of
				#we{light=none} ->
					%not ambient or light
					Acc;
				#we{light=#light{type=ambient,ambient=Amb}} ->
					Amb;
				#we{light=#light{}} ->
					Acc
			end 
		end,{0.1,0.1,0.1},gb_trees:to_list(Sh)),
	%%io:format("Ambient light ~p ~n",[Ambient]),
	gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT,Ambient),

	%% there should be a foldl over all ambient fragment programs here .. later, as defined
	%% see diffuse, of how to solve this

	%%gl:callList(Scene),
	%%executeProg(Scene,ExtObj,[ambFragProg0,ambFragProg1,ambFragProg2,ambFragProg3,ambFragProg4]),
	executeProg(Scene,ExtObj,[ambFragProg0]),

	ExtObj({ambPipePrg,Scene,ExtObj}),

	%% store into buffer
	ExtObj({add_image,Sadds+1}),

	gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT,{0,0,0,0}),
	%%io:format("Ambient light Returned error code ~p ~n",[gl:getError()]),	
	%%io:format("Ambient light GL state ~p~n",[glu:errorString(gl:getError())]),
	Sadds+1.


%
%
%
%Wrong for infinite lights
% should introduce functions for spot and pointlights 
%
%
light_render(#we{light=#light{type=ambient}},_,_,_,_)->
	ok;
light_render(#we{light=Light,vp=VP}=WE,#st{}=St,Scene,Number,ExtObj)->

	%% create and calculate the shadow map 
	%% render as fullbright into normal render
	
	
	%% will call shadow soon
	wpc_rspt_sh:shadow(WE,St,Scene,ExtObj),
	%%shadow_stencil(WE,St,Scene,ExtObj),
	
	%% done by shadow_stencil
	%%gl:clear(?GL_COLOR_BUFFER_BIT),	
	%%gl:clear(?GL_DEPTH_BUFFER_BIT),

	%% for safety (moved form material)
	gl:matrixMode(?GL_TEXTURE),
	gl:loadIdentity(),
	gl:matrixMode(?GL_MODELVIEW),
	gl:loadIdentity(),
	setup_camera_view(ExtObj),
	%% make all materials effective (done in scene render stage)
	%%ExtObj(mat_full),

	%%
	%%The state beyond this point, is
	%%     Z-buffer      : scene correct 
	%%     Color         : has none shadowed points in scene as values from 0..1 for each color
	%%                     0 in shadow, 1 in light values in between are sem shadowed by something 
	%%     accum         : storing current rendered data if selected for temp buffer
	%%     aux0          : possibly target buffer for writing
	%%     writeBuffer   : any, do not change front only is illegal however use front_and_back
	%%     ambient light : {0,0,0,0}
	%%     texture matrix: Identity
	%%     lights        : all off
	%%
	


	% has to change sooner or later ? depends on the quality and power
	% of shadow maps and misc ...
	gl:enable(?GL_LIGHT0),
	gl:enable(?GL_LIGHTING),
	%%io:format("Fetching Light .. ~p ~n",[Light]),
	#light{aim={A1,A2,A3},type=Type,diffuse=Diffuse,ambient=Ambient,specular=Specular,
		lin_att=Lin_att,quad_att=Quad_att,spot_angle=Spot_angle,spot_exp=Spot_exp,prop=Prop} = Light,
	case Type of
		ambient->
			ok; % do nothing
		point  ->
			%%io:format("Point light~n",[]),
			{X1,X2,X3} = Origin = get_average(VP),
			gl:lightfv(?GL_LIGHT0,?GL_AMBIENT,Ambient),
			gl:lightfv(?GL_LIGHT0,?GL_DIFFUSE,Diffuse),
			gl:lightfv(?GL_LIGHT0,?GL_SPECULAR,Specular),
			gl:lightfv(?GL_LIGHT0,?GL_POSITION,{X1,X2,X3,1}),
			gl:lightfv(?GL_LIGHT0,?GL_SPOT_DIRECTION,{A1-X1,A2-X2,A3-X3}),
			gl:lightf(?GL_LIGHT0,?GL_SPOT_EXPONENT,0),
			gl:lightf(?GL_LIGHT0,?GL_SPOT_CUTOFF,180.0),
			% wings does not seem to have this ... ignore for now
			%gl:lightfv(?GL_LIGHT0,?GL_CONSTANT_ATTENUATION,???),
			gl:lightf(?GL_LIGHT0,?GL_LINEAR_ATTENUATION,Lin_att),
			gl:lightf(?GL_LIGHT0,?GL_QUADRATIC_ATTENUATION,Quad_att),
			% should introduce a display state and another function handling compilation

			%%gl:callList(Scene);
			ok;
		Kind      ->
			%%io:format("Done fetching light of type ~p rendering as spotlight ~n",[Kind]),
			{X1,X2,X3} = Origin = get_average(VP),
			gl:lightfv(?GL_LIGHT0,?GL_AMBIENT,Ambient),
			gl:lightfv(?GL_LIGHT0,?GL_DIFFUSE,Diffuse),
			gl:lightfv(?GL_LIGHT0,?GL_SPECULAR,Specular),
			gl:lightfv(?GL_LIGHT0,?GL_POSITION,{X1,X2,X3,1}),
			gl:lightfv(?GL_LIGHT0,?GL_SPOT_DIRECTION,{A1-X1,A2-X2,A3-X3}),
			gl:lightf(?GL_LIGHT0,?GL_SPOT_EXPONENT,Spot_exp*128),
			gl:lightf(?GL_LIGHT0,?GL_SPOT_CUTOFF,Spot_angle),
			% wings does not seem to have this ... ignore for now
			%gl:lightfv(?GL_LIGHT0,?GL_CONSTANT_ATTENUATION,???),
			gl:lightf(?GL_LIGHT0,?GL_LINEAR_ATTENUATION,Lin_att),
			gl:lightf(?GL_LIGHT0,?GL_QUADRATIC_ATTENUATION,Quad_att),
			% should introduce a display state and another function handling compilation
			ok
			%%gl:callList(Scene)
	end,


	%%
	%% most of this is done now ... 
	%%	

	%% render all diffuse and specular programs below in a loop
	%% data should be allowed to pass to next pass ...
	%% required for faster operation and data in next pass may be useful
	%% default behaviour should be blending of plugins ...
	%% should be retrieved from the material data ?? 

	%% should recompile calllist associated with each material to render
	%% each material with specified programs
	%% material switching will be done by the gpu
	%% unfortunaly transparent, and refracting surfaces cannot be handled as easily
	%% need something more ...

	%% Could build plugins with passthrough ability, that is fast and complex
	%% fast are rendered in one scene pass, the complex are rendered by the
	%% plugin with current scene data.
	%% Should be enough to handle all materials

	%% requires multistage optimizer for this to happen
	
	%% the same things are also true for ambient light
	
	case Type of
		ambient ->
			ok;
		_       ->
			%%executeProg(Scene,ExtObj,[diffFragProg0,diffFragProg1,diffFragProg2,diffFragProg3,diffFragProg4]),
			executeProg(Scene,ExtObj,[diffFragProg0]),
			ExtObj({diffPipePrg,Scene,ExtObj})
	end,
	
	ExtObj({add_image,Number}),

	%%io:format("Light GL state ~p~n",[glu:errorString(gl:getError())]),
	% be nice
	%%gl:disable(?GL_BLEND),
	gl:disable(?GL_LIGHT0),
	ok.


%%%
%%% execute all programs for this pass
%%%
executeProg(_,_,[])->
	ok;
executeProg(Scene,ExtObj,[EL|L])->
	%% included mat_full in operator
	ExtObj({mat_full,EL}),
	gl:callList(Scene),
	executeProg(Scene,ExtObj,L).


%%
%%Top of shadow control 
%%Switch on type of light
%% incorrectly named, does not use the stencil shadow ..
%%
%shadow_stencil(#we{light=#light{type=ambient}},St,Scene,ExtObj)->
%	%% disable stencil test, do nothing 
%	ok;
%shadow_stencil(#we{light=#light{type=spot}}=WE,St,Scene,ExtObj)->
%	
%	shadow_spotlight(WE,St,Scene,ExtObj),
	%% build the shadow ...
	%% render into stencil where ??
	%% set stencil functions ...
%	ok;
%shadow_stencil(#we{light=#light{type=point}}=WE,St,Scene,ExtObj)->
%	gl:clear(?GL_COLOR_BUFFER_BIT),
	%% bail out
%	bail_out;
%shadow_stencil(#we{light=#light{type=Kind}}=WE,St,Scene,ExtObj)->
%	io:format("Ignoring unknown light kind ~p ~n",[Kind]).

%%
%%Build shadow and set stencil using spotlight
%%
%shadow_spotlight(WE,St,Scene,ExtObj)->
	%%io:format("Rendering shadow of spotlight ... ~p ~n",[WE]),
	%% recompile materials to white, should realy be alpha tex only .. ? hmm
%	ExtObj(mat_none),
%	shadow_view_depth_textured(WE,St,Scene,ExtObj),
%	ok.

%%
%%Store image in accumulation buffer
%%Render shadow view frame
%%Store in new texture (gl:genTexture(...))
%%Reconstruct Z-buffer data (render scene with clear etc)
%%Reload accum data (could replace above with recall z-buffer)
%%Apply shadow buffer how ?? (check ) 
%%Render scene as (normal)
%%

%%
%%Calculate the invers of the scene !!!
%%I could use the texture matrix here, since 
%%the operations are likely to be fast enough ...
%%Inverse here means going towards wolrd coordinates ...
%%
%%
%invert_texgen(#we{vp=VP,light=Light}=WE,St,ExtObj)->
	
	%%io:format("INvert texgen ~n",[]),

	%%
	%%Can be done differently
	%%

	%% fix one part of transform
%	gl:matrixMode(?GL_MODELVIEW),
%	gl:loadIdentity(),
%	setup_camera_view(ExtObj),
	
%	gl:matrixMode(?GL_TEXTURE),
%	gl:loadIdentity(),
	%% was 0.5,0.5
%	gl:scalef(0.5,0.5,0.5),
%	gl:translatef(1,1,1),
	
	%% transform to camera view (shadow light)
	%%io:format("INvert texgen aim ~n",[]),
%	#light{aim={A1,A2,A3},spot_angle=Angle} = Light,

	%% added part of the projection matrix
%	[W_Hither,W_Yon] = wpa:camera_info([hither,yon]),
	% should W_Hither and W_Yon be used here ?
%	glu:perspective(2*Angle,1.0,W_Hither,W_Yon),

	%%io:format("INvert texgen origin ~n",[]),
%	{X1,X2,X3} = Origin = get_average(VP),
	%% need to fix the up vector (skip lookAt ??)
	%%{U1,U2,U3} = vector_cross_product({X1,X2,X3},{A1,A2,A3}),

	%%io:format("INvert texgen beyond ~n",[]),
	%% does not do up vector properly
%	glu:lookAt(X1,X2,X3,A1,A2,A3,0,1,0),	
%	gl:matrixMode(?GL_MODELVIEW),	

%	gl:enable(?GL_TEXTURE_2D),
	%% in effect after application ??
%	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_MAG_FILTER,?GL_LINEAR),
%	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_MIN_FILTER,?GL_LINEAR),	
%	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_S,?GL_CLAMP_TO_BORDER),	%% should be clamp
%	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_T,?GL_CLAMP_TO_BORDER),
%	gl:enable(?GL_TEXTURE_GEN_S),
%	gl:enable(?GL_TEXTURE_GEN_T),
%	gl:enable(?GL_TEXTURE_GEN_R),
	%% experimental
%	gl:enable(?GL_TEXTURE_GEN_Q),
%	F = 1,
%	gl:texGenfv(?GL_S,?GL_EYE_PLANE,{1*F,0,0,0}),
%	gl:texGenfv(?GL_T,?GL_EYE_PLANE,{0,1*F,0,0}),
	%% translate -0.5 here ? was 0,0,1,0
%	gl:texGenfv(?GL_R,?GL_EYE_PLANE,{0,0,1,0}),
	%% does q matter for transform ?? 
%	gl:texGenfv(?GL_Q,?GL_EYE_PLANE,{0,0,0,1}),	
%	ok.

%%
%%Render shadow texture from eyeview (what to do here??)
%%
%shadow_view_depth_textured(Light,St,Scene,ExtObj)->


	% side effect none
	%% causing trouble ??
%	DepthMap = shadow_build(Light,St,Scene),

%	setup_camera_view(ExtObj),
%	gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),

	%% setup the modelview for viewing here check ??

	%% should introduce the reversing of coordinates of view here 
	%% as in NVIDIA code ...
	%% sets texgen functions
%	invert_texgen(Light,St,ExtObj),
			

	%% do this ?? yes
%	gl:disable(?GL_LIGHTING),

	%% what does compare do before this ... ?? 

	%% this is not causing trouble ??
%	gl:bindTexture(?GL_TEXTURE_2D,DepthMap),	
%	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_ENV_MODE,?GL_REPLACE),
%	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_COMPARE_MODE,?GL_COMPARE_R_TO_TEXTURE),
%	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_COMPARE_FUNC,?GL_LEQUAL),

%	gl:color3f(1,0,0),
%	gl:callList(Scene),

%	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_COMPARE_MODE,?GL_NONE),
	%% remove texture effects 
%	gl:bindTexture(?GL_TEXTURE_2D,0),

	%% this should produce a scene with illuminated elements	
	%% all iluminated spots should be fullbright here
	%% not true if using linear interpolation

	%% setup blend equations to multiply fragments with spot intensity

%	gl:blendEquation(?GL_FUNC_ADD),

	%% working version 
%	gl:blendFunc(?GL_DST_COLOR,?GL_ZERO),
	%% debug
	%%gl:blendFunc(?GL_ONE,?GL_ONE),
%	gl:enable(?GL_BLEND),

%	texgen_off(),	%% stop inversion

%	shadow_map_destroy(DepthMap),	
%	gl:enable(?GL_LIGHTING),
%	ok.
	

%%
%%This is shadow build but this version is only for spotlights
%% 
%%
%shadow_build(#we{light=Light,vp=VP}=WE,St,Scene)->

%	gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),	
	%% render to camera
%	shadow_camera(WE,St),
%	gl:disable(?GL_LIGHTING),
%	gl:color3f(1,1,1),
	% render lights dont do this ...
	% render backsides for better z-buffer data
%	gl:cullFace(?CULL_BACK),
	%%gl:cullFace(?GL_BACK),
	
	% should now be setup,  but we should also setup all the other stuff
%	gl:callList(Scene),

	%% create texture object, 
%	[TObj] = gl:genTextures(1),
	% this should copy the depth buffer to current texture 2d object ...
%	shadow_apply_tex(TObj),

%	io:format("Returned error code ~p ~n",[gl:getError()]),
	% be nice
%	gl:cullFace(?CULL_FRONT),
	%%gl:cullFace(?GL_FRONT),
%	gl:enable(?GL_LIGHTING),
%	TObj.


%shadow_map_destroy(TObj)->
	%% check how this looks
%	gl:deleteTextures(1,{TObj}),
%	ok.

%shadow_camera(#we{light=Light,vp=VP}=WE,St)->
	%
	%setup camera view 
	%
	%%io:format("Fetching Light .. ~p ~n",[Light]),
%	#light{aim={A1,A2,A3},spot_angle=Angle} = Light,
	%%io:format("Done fetching light~n",[]),
%	{X1,X2,X3} = Origin = get_average(VP),
	% fix projection matrix
%	Size = shadow_viewport(),
%	gl:viewport(0,0,Size,Size), % the viewport
	%% missing setup of viewport ... ??
%	gl:matrixMode(?GL_PROJECTION),
%	gl:loadIdentity(),
%	[W_Hither,W_Yon] = wpa:camera_info([hither,yon]),
	% should W_Hither and W_Yon be used here ?
	% NO, produces poor numerical results!!!
%	glu:perspective(2*Angle,1.0,W_Hither,W_Yon),
%	gl:matrixMode(?GL_MODELVIEW),
%	gl:loadIdentity(),
	%%io:format("View point {~p,~p,~p} -> {~p,~p,~p}~n",[X1,X2,X3,A1,A2,A3]),
%	glu:lookAt(X1,X2,X3,A1,A2,A3,0,1,0),
%	ok.


%%
%%Apply depth shadow texture 
%%
%shadow_apply_tex(TObj)->
	%% build binary
%	Size = get_tilesize(),
%	Val  = 0,
%	L    = Size*Size*32,
%	Bin  = <<Val:L>>,
	

	%% need to transfer data to tex before copy ?? probably ...
%	gl:bindTexture(?GL_TEXTURE_2D,TObj),
	%% assume values can be used (must in OpenGL 1.4 ??)
	%% what is target here ?? probably ?GL_TEXTURE_2D
%	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_S,?GL_CLAMP_TO_BORDER),
%	gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_T,?GL_CLAMP_TO_BORDER),
%	gl:getError(),
	%% debug version to texture surface ...
	%%gl:texImage2D(?GL_TEXTURE_2D,0,?GL_RGBA,8,8,0,?GL_RGBA,?GL_UNSIGNED_BYTE,debug_tex()),
%	gl:texImage2D(?GL_TEXTURE_2D,0,?GL_DEPTH_COMPONENT,Size,Size,0,?GL_DEPTH_COMPONENT,?GL_UNSIGNED_BYTE,Bin),
%	io:format("memory build shadow map: ~p~n",[glu:errorString(gl:getError())]),
	%% should be moved away from here
	%% the modern OpenGL 1.4 and the old versions of this
	%%gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_COMPARE_MODE,?GL_COMPARE_R_TO_TEXTURE),
	%%gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_COMPARE_FUNC,?GL_LEQUAL),

	%% copy buffer to texture object
%	gl:getError(),
%	gl:copyTexImage2D(?GL_TEXTURE_2D,0,?GL_DEPTH_COMPONENT,0,0,Size,Size,0),
	%%io:format("build shadow map: ~p~n",[glu:errorString(gl:getError())]),

%	ok.	



%%
%%Turn off texture coordinate generation, only good 
%%if we start using tex planes in inversion to depth maps ...
%%NVIDIA claims that method be faster than changin the texture matrix ...
%%Ignoring NVIDIA claims, they seem to have confused things ... 
%%
texgen_off()->

	%% reset the state of texture matrix 
	gl:matrixMode(?GL_TEXTURE),
	gl:loadIdentity(),
	gl:matrixMode(?GL_MODELVIEW),

	gl:disable(?GL_TEXTURE_GEN_S),
	gl:disable(?GL_TEXTURE_GEN_T),
	gl:disable(?GL_TEXTURE_GEN_R),
	gl:disable(?GL_TEXTURE_GEN_Q),
	gl:disable(?GL_TEXTURE_2D),
	gl:disable(?GL_TEXTURE_1D),
	ok.

%
%Return value divisable by two and not larger than view screen
% ot texture limits 
%
%shadow_viewport()->
%	get_tilesize().


%%
%%Get origin
%%
get_average(X)->
	List = gb_trees:values(X),
	{A1,A2,A3} = lists:foldl(fun({X1,X2,X3},{Y1,Y2,Y3})->
			{X1+Y1,X2+Y2,X3+Y3}
		    end,{0,0,0},List),
	L = length(List),
	{A1/L,A2/L,A3/L}.


%%%%%%%%%%%
%%%%%%%%%%% Subdivide scene as requested
%%%%%%%%%%% 


scene_subdivide(#st{shapes=SH}=St)->
	SH1 = gb_trees:to_list(SH),
	SH2 = lists:map(fun({Key,WE})->
			%io:format("scene_subdivide ~p ~n",[Key]),
			{Key,sub_divide(WE,get_pref(subdivisions,0))}
		  end,SH1),
	St#st{shapes=gb_trees:from_orddict(SH2)}.

sub_divide(WE,0)-> 
	WE;
sub_divide(WE,N)-> 
	%%io:format("Subdivisions ~p ~n",[N]),
	sub_divide(wings_subdiv:smooth(WE),N-1).


%%%%%%%%%%%
%%%%%%%%%%% Render requestor, builds data static to multiple passes
%%%%%%%%%%% 

render_req_build(St)->

	%%% build scene data with space for material recompiles
	Scene = gl:genLists(1 + wpc_rspt_ma:quantity(St)),
	wpc_rspt_ma:compile(Scene,St,full),
	gl:newList(Scene,?GL_COMPILE),
	wpc_rspt_sc:all(St,Scene),
	gl:endList(),

	%% out object
	fun(Arg)->
		case Arg of
			{mat_full,Prog} ->
				wpc_rspt_ma:compile(Scene,St,{full,Prog}),
				ok;
			mat_none->
				wpc_rspt_ma:compile(Scene,St,none),
				ok;
			mat_full->
				wpc_rspt_ma:compile(Scene,St,full),
				ok;
			scene_object->
				%%io:format("~n~n<<<<<<<<< Sending compiled scene >>>>>>>>>>>>~n Scene ~p ~n",[Scene]),
				Scene;
			delete   ->
				%%io:format("~n<<<<<<<<< deleting compiled scene >>>>>>>>>>>~n",[]),
				gl:deleteLists(Scene,1);
		
			{ambPipePrg,Scene,ExtObj} ->
				%% do something
				%%io:format("ambPipePrg called~n",[]),
				wpc_rspt_sc:all(St,Scene,ambPipe,ExtObj),			
				ok;	

			{diffPipePrg,Scene,ExtObj} ->
				%% do something
				%%io:format("diffPipePrg called~n",[]),
				wpc_rspt_sc:all(St,Scene,diffPipe,ExtObj),			
				ok;

			_       ->
				%%io:format("RenderReq  request ~p ignored~n",[Arg]),
				%% cowardly refuse request
				refuse
		end
	end.


%%%%%%%%%%%
%%%%%%%%%%% Store current part of image in some storage
%%%%%%%%%%% 

-define(INTERNAL_FORMAT,?GL_RGBA).
%%-define(INTERNAL_FORMAT,?GL_RGBA16).

%% initalize storage
img_init_storage(MTex)->
	case get_pref(sto_buffer,sto_accum) of
		sto_accum ->
				gl:clearAccum(0,0,0,1),
				gl:clear(?GL_ACCUM_BUFFER_BIT),
				ok;
		sto_texture ->
				gl:rasterPos2f(0,0),
				Size = get_tilesize(),
				Val = 0,
				L   = Size*Size*32,
				Bin = <<Val:L>>,
				%% texture has been rendered
				gl:bindTexture(?GL_TEXTURE_2D,MTex),			
				gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_MAG_FILTER,?GL_LINEAR),
				gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_MIN_FILTER,?GL_LINEAR),	
				gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_S,?GL_REPEAT),	%% should be clamp
				gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_T,?GL_REPEAT),
				gl:getError(),				
				gl:texImage2D(?GL_TEXTURE_2D,0,?INTERNAL_FORMAT,Size,Size,0,?GL_RGBA,?GL_UNSIGNED_BYTE,Bin),
				%%io:format("init storage black data GL state ~p~n",[glu:errorString(gl:getError())]),
				gl:getError(),
				gl:copyTexImage2D(?GL_TEXTURE_2D,0,?GL_RGBA,0,0,Size,Size,0),
				%%io:format("init storage GL state ~p~n",[glu:errorString(gl:getError())]),
				gl:bindTexture(?GL_TEXTURE_2D,0),
				ok
	end.

%% blend image with current stored items
img_add_image(MTex,LNumber)->

	AA = get_pref(anti_aliasing,1),

	case get_pref(sto_buffer,sto_accum) of
		sto_accum ->
			
			%% debug rubbish
			gl:color3f(1,1,1), %% odd

			%%gl:disable(?GL_VERTEX_PROGRAM_ARB),
			%%gl:disable(?GL_FRAGMENT_PROGRAM_ARB),
			io:format("Accum Lights ~p  AA=~p ~n",[LNumber,AA]),
			gl:disable(?GL_VERTEX_PROGRAM_ARB),
			gl:disable(?GL_FRAGMENT_PROGRAM_ARB),
			gl:disable(?GL_TEXTURE_2D),
			gl:disable(?GL_BLEND),
			gl:disable(?GL_DEPTH_TEST),
			gl:accum(?GL_MULT,(LNumber-1)/LNumber),
			gl:accum(?GL_ACCUM,1/(LNumber)),
			gl:enable(?GL_DEPTH_TEST),

		
			gl:enable(?GL_LIGHTING), %% should i try to restore this ??

			ok;
		sto_texture ->
			Size = get_tilesize(),
			gl:bindTexture(?GL_TEXTURE_2D,MTex),			
			%%io:format("Copy sto texture size ~p ~n",[Size]),
			gl:getError(),
			gl:blendColor(1/AA,1/AA,1/AA,1.0),
			gl:blendEquation(?GL_FUNC_ADD),
			%%io:format("add  image blend equation max GL state ~p~n",[glu:errorString(gl:getError())]),
			%% was GL_ONE GL_ONE
			gl:blendFunc(?GL_ONE,?GL_CONSTANT_COLOR),   %% replace with texture application function ??
			gl:enable(?GL_BLEND),
			img_draw_texture2d(Size,MTex),
			gl:disable(?GL_BLEND),	
			%% copy to original texture
			gl:getError(),
			gl:copyTexImage2D(?GL_TEXTURE_2D,0,?GL_RGBA,0,0,Size,Size,0),
			%%io:format("add image GL state ~p~n",[glu:errorString(gl:getError())]),
			gl:bindTexture(?GL_TEXTURE_2D,0),

			%% one color
			gl:blendColor(1.0,1.0,1.0,1.0),
			gl:blendFunc(?GL_ONE,?GL_ONE),
			ok
	end.


%% get image
img_get_image(MTex,NLights)->

	AA = get_pref(anti_aliasing,1),	

	case get_pref(sto_buffer,sto_accum) of
		sto_accum ->
			io:format("Accum Getting image with ~p lights factor ~p  AA=~p ~n",[NLights,NLights/AA,AA]),
			gl:disable(?GL_VERTEX_PROGRAM_ARB),
			gl:disable(?GL_FRAGMENT_PROGRAM_ARB),
			gl:disable(?GL_TEXTURE_2D),
			gl:disable(?GL_BLEND),
			gl:disable(?GL_DEPTH_TEST),
			gl:accum(?GL_RETURN,NLights/(AA)),
			ok;
		sto_texture ->
			Size = get_tilesize(),
			img_draw_texture2d(Size,MTex),
			%% should do color reduction if using AA
			
			gl:bindTexture(?GL_TEXTURE_2D,0),
			ok
	end.



img_draw_texture2d(Size,MTex)->
		gl:disable(?GL_VERTEX_PROGRAM_ARB),
		gl:disable(?GL_FRAGMENT_PROGRAM_ARB),
		gl:matrixMode(?GL_PROJECTION),
		gl:pushMatrix(),
		gl:loadIdentity(),
		gl:matrixMode(?GL_MODELVIEW),
		%% add sub image ?!!!
		gl:bindTexture(?GL_TEXTURE_2D,MTex),
		gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_MAG_FILTER,?GL_NEAREST),
		gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_MIN_FILTER,?GL_NEAREST),	
		gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_S,?GL_CLAMP_TO_BORDER),	%% should be clamp
		gl:texParameteri(?GL_TEXTURE_2D,?GL_TEXTURE_WRAP_T,?GL_CLAMP_TO_BORDER),
		gl:disable(?GL_LIGHTING),
		gl:pushMatrix(),
		%% eye coordinates needs this due to the multiplication with inverse of M
		gl:loadIdentity(),
		gl:enable(?GL_TEXTURE_2D),
		gl:enable(?GL_TEXTURE_GEN_S),
		gl:enable(?GL_TEXTURE_GEN_T),
		%% do not use light but use AAFActor
		gl:texEnvi(?GL_TEXTURE_ENV,?GL_TEXTURE_ENV_MODE,?GL_REPLACE),
		%% build calculation entity
		gl:texGenfv(?GL_S,?GL_EYE_PLANE,{0.5,0,0,0.5}),
		gl:texGenfv(?GL_T,?GL_EYE_PLANE,{0,0.5,0,0.5}),
		gl:disable(?GL_DEPTH_TEST),
		F = 1.0,	
		%% render texture here
		gl:'begin'(?GL_POLYGON),
		%%gl:color3f(AAFactor,AAFactor,AAFactor),
		gl:color3f(1,1,1),
		%gl:texCoord2f(-1*F*0.5+0.5,-1*F*0.5+0.5),
		gl:vertex3f(-1*F,-1*F,-0.5),
		%gl:texCoord2f(-1*F*0.5+0.5,1*F*0.5+0.5),
		gl:vertex3f(-1*F, 1*F,-0.5),
		%gl:texCoord2f(1*F*0.5+0.5,1*F*0.5+0.5),
		gl:vertex3f( 1*F, 1*F,-0.5),
		%gl:texCoord2f(1*F*0.5+0.5,-1*F*0.5+0.5),
		gl:vertex3f( 1*F,-1*F,-0.5),
		gl:'end'(),
		gl:popMatrix(),
		gl:disable(?GL_TEXTURE_2D), %% can this cause trouble ??
		gl:disable(?GL_TEXTURE_GEN_S),
		gl:disable(?GL_TEXTURE_GEN_T),
		gl:enable(?GL_DEPTH_TEST),
		gl:enable(?GL_LIGHTING), %% should i try to restore this ??
		gl:color3f(1,1,1),
		gl:matrixMode(?GL_PROJECTION),
		gl:popMatrix(),
		gl:matrixMode(?GL_MODELVIEW),
		ok.



debug_tex()->
	X= <<255:8,0:8,0:8,255:8>>,
	S= <<0:8,0:8,0:8,255:8>>,
	List = [X,X,X,X,X,X,X,X,
		X,X,S,S,S,X,X,X,
		X,S,X,S,S,X,S,X,
		X,S,S,X,X,S,S,X,
	
		X,S,S,X,X,S,S,X,
		X,S,X,S,S,X,S,X,
		X,X,S,S,S,S,X,X,
		X,X,X,X,X,X,X,X],
	list_to_binary(List).
