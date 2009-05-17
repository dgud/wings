%%
%%  wings.hrl --
%%
%%     Global record definition and defines.
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
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

-ifdef(NEED_OPENGL).
-include_lib("esdl/include/gl.hrl").
-include_lib("esdl/include/glu.hrl").
-endif.

-define(WINGS_VERSION, ?wings_version).

-define(CHAR_HEIGHT, wings_text:height()).
-define(CHAR_WIDTH, wings_text:width()).

-define(LINE_HEIGHT, (?CHAR_HEIGHT+2)).
-define(GROUND_GRID_SIZE, 1).
-define(CAMERA_DIST, (8.0*?GROUND_GRID_SIZE)).
-define(NORMAL_LINEWIDTH, 1.0).
-define(DEGREE, 176).				%Degree character.

-define(HIT_BUF_SIZE, (1024*1024)).

-define(PANE_COLOR, {0.52,0.52,0.52}).
-define(BEVEL_HIGHLIGHT, {0.9,0.9,0.9}).
-define(BEVEL_LOWLIGHT, {0.3,0.3,0.3}).
-define(BEVEL_HIGHLIGHT_MIX, 0.5).
-define(BEVEL_LOWLIGHT_MIX, 0.5).

-define(SLOW(Cmd), begin wings_io:hourglass(), Cmd end).
-define(TC(Cmd), wings_util:tc(fun() -> Cmd end, ?MODULE, ?LINE)).

-ifdef(DEBUG).
-define(ASSERT(E), case E of
		       true -> ok;
		       _ ->
			   erlang:error({assertion_failed,?MODULE,?LINE})
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
	 pick=none,				%For picking.
	 proxy_faces=none,			%Smooth proxy faces.
	 proxy_edges=none,			%Smooth proxy edges.

	 %% Miscellanous.
	 hilite=none,				%Hilite display list.
	 mirror=none,				%Virtual mirror data.
	 ns=none,				%Normals/positions per face.
	 plugins=[],                %Draw lists for plugins.

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

%% Main state record containing all objects and other important state.
-record(st,
	{shapes=gb_trees:empty() :: gb_tree(),	%All visible objects
	 selmode=face :: sel_mode(),		%Selection mode.
	 sh=false :: bool(),			%Smart highlighting active.
	 sel=[],				%Current sel: [{Id,GbSet}]
	 ssels=gb_trees:empty() :: gb_tree(),   %Saved selections:

	 %% Selection only temporary?
	 temp_sel=none :: 'none' | {sel_mode(),bool()},

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
	 next_is_undo=true :: bool(),		%State of undo/redo toggle.
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
	 fs :: gb_tree(),		        %Faces
	 he=gb_sets:empty() :: gb_set(),	%Hard edges
	 vc :: gb_tree(),	                %Connection info (=incident edge)
						% for vertices.
	 vp=gb_trees:empty() :: gb_tree(),	%Vertex positions.
	 pst=gb_trees:empty(),                  %Plugin State Info, 
						%   gb_tree where key is plugin module
	 mat=default,				%Materials.
	 next_id=0 :: non_neg_integer(),	%Next free ID for vertices,
						% edges, and faces.
						% (Needed because we never re-use
						%  IDs.)
	 mode=material :: 'vertex'|'material'|'uv',
	 mirror=none :: 'none' | non_neg_integer(),	%Mirror: none|Face
	 light=none,				%Light data: none|Light
	 has_shape=true :: bool()		%true|false
	}).

-define(IS_VISIBLE(Perm), (Perm =< 1)).
-define(IS_NOT_VISIBLE(Perm), (Perm > 1)).
-define(IS_SELECTABLE(Perm), (Perm == 0)).
-define(IS_NOT_SELECTABLE(Perm), (Perm =/= 0)).

-define(IS_LIGHT(We), ((We#we.light =/= none) and (not We#we.has_shape))).
-define(IS_ANY_LIGHT(We), (We#we.light =/= none)).
-define(HAS_SHAPE(We), (We#we.has_shape)).

%% Edge in a winged-edge shape.
-record(edge,
	{vs=0 :: vertex_num(),			%Start vertex for edge
	 ve=0 :: vertex_num(),			%End vertex for edge
	 a=none,			        %Color or UV coordinate.
	 b=none,			        %Color or UV coordinate.
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

