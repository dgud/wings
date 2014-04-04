%%
%%  wings.hrl --
%%
%%     Global record definition and defines.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-include("../intl_tools/wings_intl.hrl").

-ifdef(NEED_ESDL).
-include_lib("esdl/include/sdl.hrl").
-include_lib("esdl/include/sdl_events.hrl").
-include_lib("esdl/include/sdl_video.hrl").
-include_lib("esdl/include/sdl_keyboard.hrl").
-include_lib("esdl/include/sdl_mouse.hrl").
-include_lib("esdl/src/sdl_util.hrl").
-define(CTRL_BITS, ?KMOD_CTRL).
-define(ALT_BITS, ?KMOD_ALT).
-define(SHIFT_BITS, ?KMOD_SHIFT).
-define(META_BITS, ?KMOD_META).
-endif.

%% Some macros used when we change esdl version
%% should be cleaned up, removed when everything 
%% works.  (See also wings_gl)

-ifdef(NEED_OPENGL).
-ifndef(NEED_ESDL).
-include_lib("esdl/include/sdl.hrl"). %% We need SDL_USES_WX_GL
-endif.

-ifdef(SDL_USES_WX_GL).
-define(USE_WX_OPENGL, 1).
-endif.

-ifndef(USE_WX_OPENGL).
-include_lib("esdl/include/gl.hrl").
-include_lib("esdl/include/glu.hrl").
-else.
-include_lib("wx/include/gl.hrl").
-include_lib("wx/include/glu.hrl").
-endif.
-endif.

-ifdef(USE_WX).
-include_lib("wx/include/wx.hrl").
-endif.

-define(WINGS_VERSION, ?wings_version).

-define(CHAR_HEIGHT, wings_text:height()).
-define(CHAR_WIDTH, wings_text:height() div 2). %% because of double wide Chinese fonts

-define(LINE_HEIGHT, (?CHAR_HEIGHT+2)).
-define(GROUND_GRID_SIZE, 1).
-define(GROUND_GRID_AMOUNT, 10).
-define(NORMAL_LINEWIDTH, 1.0).
-define(DEGREE, 176).				%Degree character.

-define(F32,  32/float-native).
-define(I32,  32/signed-native).
-define(UI32, 32/native).

-define(PANE_COLOR, {0.52,0.52,0.52}).
-define(BEVEL_HIGHLIGHT, {0.9,0.9,0.9}).
-define(BEVEL_LOWLIGHT, {0.3,0.3,0.3}).
-define(BEVEL_HIGHLIGHT_MIX, 0.5).
-define(BEVEL_LOWLIGHT_MIX, 0.5).

%% wings io defs
-define(ICON_WIDTH, 32).
-define(ICON_HEIGHT, 28).

-define(TX_WIDTH, 256).
-define(TX_HEIGHT, 128).

-define(ACTIVE_TX, wings_io_active_tx).

-record(io,
	{tex=[],				%Textures.
	 grab_count=0,				%Number of grabs.
	 cursors,				%Mouse cursors.
	 raw_icons				%Raw icon bundle.
	}).

-define(EVENT_QUEUE, wings_io_event_queue).

-define(DIFFUSE_MAP_UNIT, 0).
-define(NORMAL_MAP_UNIT,  1).
-define(ENV_MAP_UNIT,     2).
-define(TANGENT_ATTR,     5).
%%

-define(SLOW(Cmd), begin wings_io:hourglass(), Cmd end).
-define(TC(Cmd), wings_util:tc(fun() -> Cmd end, ?MODULE, ?LINE)).

-ifdef(DEBUG).
-define(ASSERT(E), case E of
		       true -> ok;
		       _ ->
			   error({assertion_failed,?MODULE,?LINE})
		   end).
-define(CHECK_ERROR(), wings_gl:check_error(?MODULE, ?LINE)).
-else.
-define(ASSERT(E),ok).
-define(CHECK_ERROR(), ok).
-endif.

%%
%% Types.
%%
-type vertex_num() :: non_neg_integer().
-type edge_num() :: non_neg_integer().
-type face_num() :: integer().
-type visible_face_num() :: non_neg_integer().
-type elem_num() :: vertex_num() | edge_num() | face_num().

-type sel_mode() :: 'vertex' | 'edge' | 'face' | 'body'.
-type bounding_box() :: [{float(),float(),float()}].

-type wings_cmd() :: tuple() | atom().
-type maybe_wings_cmd() :: 'ignore' | wings_cmd().

-type wings_vtx_buffer() :: 'none' | {integer(),binary()}.


%% State and records
%% Main state record containing all objects and other important state.
-record(st,
	{shapes=gb_trees:empty() :: gb_tree(),	%All visible objects
	 selmode=face :: sel_mode(),		%Selection mode.
	 sh=false :: boolean(),			%Smart highlighting active.
	 sel=[],				%Current sel: [{Id,GbSet}]
	 ssels=gb_trees:empty() :: gb_tree(),   %Saved selections:

	 %% Selection only temporary?
	 temp_sel=none :: 'none' | {sel_mode(),boolean()},

	 mat=gb_trees:empty() :: gb_tree(),	%Defined materials (GbTree).
	 pal=[],                                %Palette
	 file,					%Current filename.
	 saved=false :: 'false'  | 'true' | 'auto' | integer(),
	 onext=1 :: pos_integer(),		%Next object id to use.

	 %% Saved bounding box. (AutoUV uses it for its own purposes,
	 %% therefore the type must also a allow a tuple.)
	 bb=none :: 'none' | bounding_box() | tuple(),

	 edge_loop=none,			%Previous edge loop.
	 views={0,{}},				%{Current,TupleOfViews}
	 pst=gb_trees:empty() :: gb_tree(),     %Plugin State Info
						%   gb_tree where key is plugin	module 

	 %% Previous commands.
	 repeatable=ignore :: maybe_wings_cmd(), %Last repeatable command.
	 ask_args,				%Ask arguments.
	 drag_args,			        %Drag arguments for command.

	 %% Default commands (LMB, RMB).
	 def={ignore,ignore} :: {maybe_wings_cmd(),maybe_wings_cmd()},


	 %% Undo information.
	 last_cmd=empty_scene,		        %Last command.
	 undo=queue:new() :: queue(),		%Undo (de)queue.
	 next_is_undo=true :: boolean(),	%State of undo/redo toggle.
	 undone=[] :: list()		        %States that were undone.
	}).

%% The Winged-Edge data structure.
%% See http://www.cs.mtu.edu/~shene/COURSES/cs3621/NOTES/model/winged-e.html
-record(we,
	{id :: non_neg_integer(),		%Shape id.
	 perm=0,				%Permissions:
						% 0 - Everything allowed.
						% 1 - Visible, can't select.
						% [] or {Mode,GbSet} -
						%  Invisible, can't select.
						%  The GbSet contains the
						%  object's selection.
	 name="" :: string() | tuple(),		%Name. (AutoUV stores other things here.)
	 es=array:new() :: array(),		%array containing edges
	 lv=none :: 'none' | array(),	        %Left vertex attributes
	 rv=none :: 'none' | array(),	        %Right vertex attributes,
	 fs :: gb_tree(),		        %Faces
	 he=gb_sets:empty() :: gb_set(),	%Hard edges
	 vc :: array(),		                %Connection info (=incident edge)
						% for vertices.
	 vp=array:new() :: array(),		%Vertex positions.
	 pst=gb_trees:empty(),                  %Plugin State Info, 
						%   gb_tree where key is plugin module
	 mat=default,				%Materials.
	 next_id=0 :: non_neg_integer(),	%Next free ID for vertices,
						% edges, and faces.
						% (Needed because we never re-use
						%  IDs.)
	 mirror=none :: 'none' | non_neg_integer(),	%Mirror: none|Face
	 light=none,				%Light data: none|Light
	 holes=[] :: [integer()]		%List of hole faces.
	}).

-define(IS_VISIBLE(Perm), (Perm =< 1)).
-define(IS_NOT_VISIBLE(Perm), (Perm > 1)).
-define(IS_SELECTABLE(Perm), (Perm =:= 0)).
-define(IS_NOT_SELECTABLE(Perm), (Perm =/= 0)).

%%
%% Macros for testing for lights. We don't want to put the record
%% definition for the light record here (to discourage plug-in
%% writes bypassing the API in wings_light), so we'll depend on
%% the type of light to be in the second element of the tuple.
%%
-define(IS_ANY_LIGHT(We), (We#we.light =/= none)).
-define(IS_LIGHT(We), (We#we.light =/= none andalso
		       element(2, We#we.light) =/= area)).
-define(IS_AREA_LIGHT(We), (We#we.light =/= none andalso
			    element(2, We#we.light) =:= area)).

%% Edge in a winged-edge object.
%%
%%                \       /           
%%                 \     /            
%%            ltpr  \   / rtsu        
%%                   \ /              
%%                   ve  b            
%%                    |               
%%                    |               
%%       lf           |          rf   
%%                    |               
%%                    |               
%%                 a  vs              
%%                   / \              
%%            ltsu  /   \ rtpr        
%%                 /     \            
%%                /       \           
%%                               	   
-record(edge,
	{vs=0 :: vertex_num(),			%Start vertex for edge
	 ve=0 :: vertex_num(),			%End vertex for edge
	 lf=0 :: face_num(),			%Left face
	 rf=0 :: face_num(),			%Right face
	 ltpr=0 :: edge_num(),			%Left traversal predecessor
	 ltsu=0 :: edge_num(),			%Left traversal successor
	 rtpr=0 :: edge_num(),			%Right traversal predecessor
	 rtsu=0	:: edge_num()			%Right traversal successor
	}).

%% The current view/camera.
-record(view,
	{origin,
	 distance,				% From origo.
	 azimuth,
	 elevation,
	 pan_x,					%Panning in X direction.
	 pan_y,					%Panning in Y direction.
	 along_axis=none,			%Which axis viewed along.
	 fov,					%Field of view.
	 hither,				%Near clipping plane.
	 yon					%Far clipping plane.
	}).


%% Display lists per object.
%%  Important: Plain integers and integers in lists will be assumed to
%%  be display lists. Arbitrary integers must be stored inside a tuple
%%  or record to not be interpreted as a display list.
-record(dlo,
	{work=none,				%Workmode faces.
	 smooth=none,				%Smooth-shaded faces.
	 edges=none,				%Edges and wire-frame.
	 vs=none,				%Unselected vertices.
	 hard=none,				%Hard edges.
	 sel=none,				%Selected items.
	 orig_sel=none,				%Original selection.
	 normals=none,				%Normals.

	 vab     = none,                        %% Vertex array (see below)
	 tri_map = none,			%Tri -> Face map (for picking)

	 %% Miscellanous.
	 hilite=none,				%Hilite display list.
	 mirror=none,				%Virtual mirror data.
	 ns=none,				%Normals/positions per face.
	 plugins=[],                            %Draw lists for plugins.
	 proxy=false,                           %Proxy Data is used.

	 %% Source for display lists.
	 src_we=none,				%Source object.
	 src_sel=none,				%Source selection.
	 orig_mode=none,			%Original selection mode.
	 split=none,				%Split data.
	 drag=none,				%For dragging.
	 transparent=false,			%Object includes transparancy.
	 proxy_data=none,			%Data for smooth proxy.
	 open=false,				%Open (has hole).

	 %% List of display lists known to be needed only based
	 %% on display modes, not whether the lists themselves exist.
	 %% Example: [work,edges]
	 needed=[]
	}).

%% Vertex array buffers
-record(vab, 
	{
	  %% Vertex buffers. Each vertex buffer looks like
	  %% {Stride,Binary}, where Stride is the stride to be
	  %% used when setting up the vertex buffer.
	  face_vs  = none :: wings_vtx_buffer(), %Vertex binary for drawing faces
	  face_fn  = none :: wings_vtx_buffer(), %Face Normals (flat but per vertex)
	  face_sn  = none :: wings_vtx_buffer(), %Face Normals (smooth)
	  face_uv  = none :: wings_vtx_buffer(), %UV coords
	  face_ts  = none :: wings_vtx_buffer(), %Tangent vector
	  face_vc  = none :: wings_vtx_buffer(), %Vertex Colors coords
	  face_es  = none :: wings_vtx_buffer(), %Edges 2*Vertex coords
	  face_map = none,                       %FaceId -> {BinPos,TriCount}
	  mat_map  = none                        %Face per Material draw info
	 }).

