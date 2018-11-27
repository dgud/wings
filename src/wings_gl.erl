%%
%%  wings_gl.erl --
%%
%%     A few OpenGL utilities.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_gl).
-export([init/1, window/4, attributes/0,
	 is_ext/1,is_ext/2,
	 is_restriction/1,
	 error_string/1]).

%% GLSL exports
-export([support_shaders/0,
         use_prog/1,
	 uloc/2, set_uloc/3,
	 compile/2, link_prog/1, link_prog/2]).

%% FBO exports
-export([have_fbo/0, setup_fbo/2, delete_fbo/1]).

%% GL wrappers
-export([project/6, unProject/6,
	 triangulate/2, deleteTextures/1,
	 bindFramebuffer/2,
	 drawElements/4
	]).

%% Debugging.
-export([check_error/2]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-ifndef(WX_GL_SAMPLE_BUFFERS).     %% New in wxWidgets-3.0
-define(WX_GL_SAMPLE_BUFFERS,17).  %% 1 for multisampling support (antialiasing)
-define(WX_GL_SAMPLES,18).         %% 4 for 2x2 antialiasing supersampling on most graphics cards
-endif.

init(Parent) ->
    GL = window(Parent, undefined, true, true),
    wx_object:cast(Parent, {init_menus, Parent}),
    init_extensions(),
    init_restrictions(),
    GL.

attributes() ->
    SB = case os:type() of
             {unix, Os} when Os =/= darwin ->
                 %% Sample buffers does not currently work on Wayland
                 case os:getenv("XDG_SESSION_TYPE") of
                     "x11" -> [?WX_GL_SAMPLE_BUFFERS,1, ?WX_GL_SAMPLES,4];
                     "X11" -> [?WX_GL_SAMPLE_BUFFERS,1, ?WX_GL_SAMPLES,4];
                     _ -> []
                 end;
             _ ->
                 [?WX_GL_SAMPLE_BUFFERS,1, ?WX_GL_SAMPLES,4]
         end,
    {attribList,
     [?WX_GL_RGBA,
      ?WX_GL_MIN_RED,8,?WX_GL_MIN_GREEN,8,?WX_GL_MIN_BLUE,8,
      ?WX_GL_DEPTH_SIZE, 24,
      ?WX_GL_DOUBLEBUFFER] ++
         SB ++ [0]
    }.

window(Parent, Context, Connect, Show) ->
    Style = ?wxFULL_REPAINT_ON_RESIZE bor ?wxWANTS_CHARS,
    Flags = [attributes(), {style, Style}],
    GL = case Context of
	     undefined ->
		 Win = wxGLCanvas:new(Parent, Flags),
		 ?SET(gl_canvas, Win),
		 Win;
	     _ ->
		 wxGLCanvas:new(Parent, Context, Flags)
	 end,
    Connect andalso connect_events(GL),
    case Show of
	true ->
	    wxWindow:connect(Parent, show),
	    wxFrame:show(Parent),
	    receive #wx{event=#wxShow{}} -> ok end;
	false ->
	    ok
    end,
    wxGLCanvas:setCurrent(GL),
    wxWindow:disconnect(Parent, show),
    GL.

%% Event handling for OpenGL windows

connect_events(Canvas) ->
    %% Re-attaches the OpenGL Context to Window when is [re] created/showed
    wxWindow:connect(Canvas, create,
                     [{callback, fun(_, _) -> wxGLCanvas:setCurrent(Canvas) end}]),
    wxWindow:connect(Canvas, show,
                     [{callback, fun(#wx{event=#wxShow{show=Show}}, _) ->
                                         Show andalso (catch wxGLCanvas:setCurrent(Canvas)) end}]),
    case os:type() of
	{unix, darwin} ->
	    wxWindow:connect(Canvas, paint, [{callback, fun redraw/2}]);
	{unix, _} ->
	    wxWindow:connect(Canvas, paint, [{skip, true}]),
	    ok;
	{win32, _} ->
	    wxWindow:connect(Canvas, paint, [{callback, fun redraw/2}]),
	    wxWindow:connect(Canvas, erase_background, [{callback, fun redraw/2}])
    end,

    wxWindow:connect(Canvas, size),
    catch wxWindow:connect(Canvas, mouse_capture_lost), %% Not available in old wx's.

    setup_std_events(Canvas),
    wxWindow:setFocus(Canvas), %% Get keyboard focus
    wxFrame:dragAcceptFiles(Canvas, true),
    wxFrame:connect(Canvas, drop_files),
    ok.

redraw(#wx{obj=Canvas, event=#wxPaint{}}=Ev,_) ->
    %% Must do a PaintDC and destroy it
    DC = wxPaintDC:new(Canvas),
    wxPaintDC:destroy(DC),
    wings ! Ev,
    ok;
redraw(Ev, _) ->  %% For erase background events
    wings ! Ev#wx{event=#wxPaint{type=paint}}.

setup_std_events(Canvas) ->
    wxWindow:connect(Canvas, motion),
    wxWindow:connect(Canvas, left_up),
    wxWindow:connect(Canvas, left_down),
    wxWindow:connect(Canvas, middle_up),
    wxWindow:connect(Canvas, middle_down),
    wxWindow:connect(Canvas, left_dclick),
    wxWindow:connect(Canvas, right_up),
    wxWindow:connect(Canvas, right_down),
    wxWindow:connect(Canvas, mousewheel),
    %% wxWindow:connect(Canvas, char_hook, []),
    wxWindow:connect(Canvas, key_down, [{callback, fun key_callback_win32/2}]),
    wxWindow:connect(Canvas, key_up), %% Normally suppressed
    wxWindow:connect(Canvas, kill_focus, [{skip, true}]),
    wxWindow:connect(Canvas, char).

key_callback_win32(Ev = #wx{event=Key=#wxKey{rawFlags=Raw}},Obj) ->
    %% See WM_SYSKEYDOWN message in msdn
    %% https://msdn.microsoft.com/en-us/library/windows/desktop/ms646286(v=vs.85).aspx
    Repeat = (Raw band (1 bsl 30)) > 1,
    %% AltGr  = (Raw band (1 bsl 24)) > 1,
    %% Repeat orelse io:format("Ev ~p~n   ~.2B => Repeat ~p AltGr ~p~n",
    %%  			    [Key, Raw, Repeat, AltGr]),
    case forward_key(Key) of
	true when Repeat -> ignore;
	%% true when AltGr -> ignore;
	true -> wings ! Ev;
	false -> wxEvent:skip(Obj)
    end.

forward_key(#wxKey{controlDown=true}) -> true;
forward_key(#wxKey{altDown=true}) -> true;
forward_key(#wxKey{metaDown=true}) -> true;
forward_key(#wxKey{shiftDown=true, keyCode=?WXK_SHIFT}) -> true;
forward_key(_) -> false.


%%%
%%% OpenGL extensions.
%%%
init_extensions() ->
    ets:new(wings_gl_ext, [named_table,public,ordered_set]),
    Exts0 = lists:sort(string:tokens(gl:getString(?GL_EXTENSIONS), " ")),
    Exts = [{list_to_atom(E)} || E <- Exts0],
    ets:insert(wings_gl_ext, Exts),
    Ver = case catch get_version() of
	      {_,_,_}=V -> V;
	      _ -> {1,1,0}
	  end,
    ets:insert(wings_gl_ext, {version,Ver}).

get_version() ->
    case string:tokens(gl:getString(?GL_VERSION), ". ") of
	[Major0,Minor0] ->
	    Patch = 0;
	[Major0,Minor0,Patch0|_] ->
	    case catch list_to_integer(Patch0) of
		{'EXIT',_} -> Patch = 0;
		Patch -> Patch
	    end
    end,
    Major = list_to_integer(Major0),
    Minor = list_to_integer(Minor0),
    {Major,Minor,Patch}.

%% Either check for a given version (or higher), or
%% for that all the given extensions are implemented.
is_ext(Wanted) when tuple_size(Wanted) >= 2 ->
    [{_,Actual}] = ets:lookup(wings_gl_ext, version),
    version_match(Wanted, Actual);
is_ext(Ext) when is_atom(Ext); is_list(Ext) ->
    is_ext_1(Ext).

%% Must be Wanted version or higher, or the List of extensions must match.
is_ext(Wanted, []) ->
    is_ext(Wanted);
is_ext(Wanted, List) ->
    is_ext(Wanted) orelse is_ext_1(List).

is_ext_1([]) ->
    true;
is_ext_1([Name|R]) ->
    is_ext_1(Name) andalso is_ext(R);
is_ext_1(Name) ->
    ets:member(wings_gl_ext, Name).

version_match({Ma1,_}, {Ma2,_,_})
  when Ma1 < Ma2 -> 
    true;
version_match({Ma1,Mi1}, {Ma2,Mi2,_}) 
  when Ma1 =< Ma2, Mi1 =< Mi2 -> true;
version_match({Ma1,Mi1,P1}, {Ma2,Mi2,P2}) 
  when Ma1 =< Ma2, Mi1 =< Mi2, P1 =< P2 -> true;
version_match(_,_) ->
    false.

%%%
%%% OpenGL restrictions (bugs and limitations).
%%%
init_restrictions() ->
    ets:new(wings_gl_restriction, [named_table,public,ordered_set]),
    case os:type() of
	{unix,sunos} ->
	    %% Scissor does not work for clipping text.
	    ets:insert(wings_gl_restriction, [{broken_scissor}]);
	_ ->
	    ok
    end.

is_restriction(Name) ->
    ets:member(wings_gl_restriction, Name).

error_string(0) -> no_error;
error_string(?GL_INVALID_VALUE) -> "GL_INVALID_VALUE";
error_string(?GL_INVALID_ENUM) -> "GL_INVALID_ENUM";
error_string(?GL_INVALID_OPERATION) -> "GL_INVALID_OPERATION";
error_string(?GL_STACK_OVERFLOW) -> "GL_STACK_OVERFLOW";
error_string(?GL_STACK_UNDERFLOW) -> "GL_STACK_UNDERFLOW";
error_string(?GL_OUT_OF_MEMORY) -> "GL_OUT_OF_MEMORY";
error_string(Error) -> "Error: "++integer_to_list(Error).

%%%
%%% Error checking in debug builds.
%%%

-ifdef(DEBUG).
check_error(Mod, Line) ->
    case error_string(gl:getError()) of
	no_error ->
	    ok;
	Str ->
	    io:format("~p, line ~p: ~s\n", [Mod,Line,Str]),
	    error(gl_error, [Mod,Line])
    end.
-else.
check_error(_Mod, _Line) ->
    ok.
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Shader compilation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

support_shaders() ->
    true. %% Required in 2.0

use_prog(Prog) when is_integer(Prog) ->
    gl:useProgram(Prog).

uloc(Prog, What) ->
    try gl:getUniformLocation(Prog, What) of
	-1 -> -1;
	Where -> Where
    catch _:_ ->
	    ErrStr = "ERROR The uniform variable ~p should be a string~n",
	    Err = io_lib:format(ErrStr, [What]),
	    throw(lists:flatten(Err))
    end.

set_uloc(#{}=Map, Var, Val) ->
    case maps:get(Var,Map, undefined) of
        undefined -> %io:format("~p: NO ~p~n~p~n",[?LINE,Var, Map]),
            ok;
        Pos ->
            set_uloc(Pos, Val)
    end;
set_uloc(Prog, Var, Val) ->
    case uloc(Prog, Var) of
	-1 ->  ok;
	Pos -> set_uloc(Pos,Val)
    end.

set_uloc(Pos, A) when is_integer(A) ->
    gl:uniform1i(Pos,A);
set_uloc(Pos, A) when is_float(A) ->
    gl:uniform1f(Pos,A);
set_uloc(Pos, {A,B}) when is_float(A),is_float(B) ->
    gl:uniform2f(Pos,A,B);
set_uloc(Pos, {A,B,C}) when is_float(A),is_float(B),is_float(C) ->
    gl:uniform3f(Pos,A,B,C);
set_uloc(Pos, {A,B,C,D}) when is_float(A),is_float(B),is_float(C) ->
    gl:uniform4f(Pos,A,B,C,D);
set_uloc(Pos, [{A,B,C}|_]=L) when is_float(A),is_float(B),is_float(C) ->
    gl:uniform3fv(Pos,L);
set_uloc(Pos, [{A,B,C,_}|_]=L) when is_float(A),is_float(B),is_float(C) ->
    gl:uniform4fv(Pos,L);
set_uloc(Pos, Mat) when tuple_size(Mat) =:= 9 ->
    gl:uniformMatrix3fv(Pos, 0, [Mat]);
set_uloc(Pos, Mat) when tuple_size(Mat) =:= 16 ->
    gl:uniformMatrix4fv(Pos, 0, [Mat]);
set_uloc(Pos, Mat) when tuple_size(Mat) =:= 12 ->
    gl:uniformMatrix3x4fv(Pos, 1, [Mat]).

compile(Type, Src) when is_binary(Src) ->
    Handle = gl:createShader(glType(Type)),
    ok = gl:shaderSource(Handle, [Src]),
    ok = gl:compileShader(Handle),
    case gl:getShaderiv(Handle, ?GL_COMPILE_STATUS) of
        ?GL_TRUE  -> Handle;
        ?GL_FALSE ->
            BufSize = gl:getShaderiv(Handle, ?GL_INFO_LOG_LENGTH),
            ErrorStr = gl:getShaderInfoLog(Handle, BufSize),
            io:format("Error: in ~p shader~n: ~s~n",[Type, ErrorStr]),
            gl:deleteShader(Handle),
            throw({Type, "Compilation failed"})
    end.

glType(vertex) -> ?GL_VERTEX_SHADER;
glType(fragment) -> ?GL_FRAGMENT_SHADER.

link_prog(Objs) ->
    link_prog(Objs, []).
link_prog(Objs, Attribs) when is_list(Objs) ->
    Prog = gl:createProgram(),
    [gl:attachShader(Prog,ObjCode) || ObjCode <- Objs],
    [gl:deleteShader(ObjCode) || ObjCode <- Objs],
    %% Must be bound before link (if any)
    [gl:bindAttribLocation(Prog, Idx, AttribName) || {Idx, AttribName} <- Attribs],
    gl:linkProgram(Prog),
    case gl:getProgramiv(Prog, ?GL_LINK_STATUS) of
        ?GL_TRUE  -> Prog;
        ?GL_FALSE ->
            BufSize = gl:getProgramiv(Prog, ?GL_INFO_LOG_LENGTH),
            ErrorStr = gl:getProgramInfoLog(Prog, BufSize),
            io:format("Error: in program linking~n: ~s~n",[ErrorStr]),
            gl:deleteProgram(Prog),
            throw({link, "Linking failed"})
    end.

%%%%%%%%%%%%%  Framebuffer object %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

have_fbo() ->
    is_ext('GL_ARB_framebuffer_object') orelse is_ext('GL_EXT_framebuffer_object').

%% Size = {W,H}
%% What = {BufferType, Options}
%%   => [{BufferType, Identifer}] | not_supported
setup_fbo(Size, What) ->
    case have_fbo() of
	false -> not_supported;
	true -> setup_fbo_1(Size, What)
    end.

setup_fbo_1(Size, Types) ->
    [FB] = gl:genFramebuffers(1),
    gl:bindFramebuffer(?GL_FRAMEBUFFER, FB),
    {Bfs,_} = lists:foldl(fun(What, {Acc, ColCount}) ->
				  case setup_fbo_2(What, Size, ColCount) of
				      {color, _} = Res ->
					  {[Res|Acc], ColCount+1};
				      Res ->
					  {[Res|Acc], ColCount}
				  end
			  end, {[],0}, Types),
    Buffers = [{fbo, FB}|lists:reverse(Bfs)],
    case check_fbo_status(FB) of
	false ->
	    delete_fbo(Buffers),
	    not_supported;
	_ ->
	    Buffers
    end.

setup_fbo_2({color, Options}, {W,H}, Count) ->
    %% Init color texture
    [Col] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, Col),
    Internal = proplists:get_value(internal, Options, ?GL_RGBA8),
    Format = proplists:get_value(format, Options, ?GL_RGBA),
    Type = proplists:get_value(type, Options, ?GL_UNSIGNED_BYTE),
    gl:texImage2D(?GL_TEXTURE_2D, 0, Internal, W, H, 0, Format, Type, 0),

    MinF = proplists:get_value(min, Options, ?GL_LINEAR),
    gl:texParameterf(?GL_TEXTURE_2D,?GL_TEXTURE_MIN_FILTER, MinF),
    MagF = proplists:get_value(mag, Options, ?GL_LINEAR),
    gl:texParameterf(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, MagF),

    WS = proplists:get_value(wrap_s, Options, ?GL_REPEAT),
    gl:texParameterf(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, WS),
    WT = proplists:get_value(wrap_t, Options, ?GL_REPEAT),
    gl:texParameterf(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, WT),

    GEN_MM = proplists:get_value(gen_mipmap, Options, ?GL_FALSE),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_GENERATE_MIPMAP, GEN_MM),
    if GEN_MM =:= ?GL_TRUE ->
	    gl:generateMipmap(?GL_TEXTURE_2D);
       true -> ok
    end,

    gl:framebufferTexture2D(?GL_FRAMEBUFFER_EXT,
			     ?GL_COLOR_ATTACHMENT0_EXT + Count,
			     ?GL_TEXTURE_2D, Col, 0),
    {color, Col};
setup_fbo_2({depth, Options}, {W,H}, _) ->
    [Depth] = gl:genRenderbuffers(1),
    %% Init depth texture
    gl:bindRenderbuffer(?GL_RENDERBUFFER_EXT, Depth),
    Internal = proplists:get_value(internal, Options, ?GL_DEPTH_COMPONENT24),
    gl:renderbufferStorage(?GL_RENDERBUFFER_EXT,Internal, W, H),
    gl:framebufferRenderbuffer(?GL_FRAMEBUFFER_EXT,
				?GL_DEPTH_ATTACHMENT_EXT,
				?GL_RENDERBUFFER_EXT, Depth),
    {depth, Depth}.

delete_fbo(List) ->
    gl:framebufferTexture2D(?GL_FRAMEBUFFER_EXT,
			     ?GL_COLOR_ATTACHMENT0_EXT,
			     ?GL_TEXTURE_2D,0,0),
    Textures = [Col || {color, Col} <- List],
    deleteTextures(Textures),
    gl:framebufferRenderbuffer(?GL_FRAMEBUFFER_EXT,
				?GL_DEPTH_ATTACHMENT_EXT,
				?GL_RENDERBUFFER_EXT, 0),
    Depth = [D || {depth, D} <- List],
    deleteRenderbuffers(Depth),
    gl:bindFramebuffer(?GL_FRAMEBUFFER_EXT, 0),
    FB = [F || {framebuffer, F} <- List],
    deleteFramebuffers(FB).

check_fbo_status(FB) ->
    case gl:checkFramebufferStatus(?GL_FRAMEBUFFER_EXT) of
	?GL_FRAMEBUFFER_COMPLETE_EXT ->
	    FB;
	?GL_FRAMEBUFFER_UNSUPPORTED_EXT ->
	    io:format("GL_FRAMEBUFFER_UNSUPPORTED_EXT~n",[]),
	    false;
	?GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT ->
	    io:format("GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT~n",[]),
	    false;
        ?GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT ->
	    io:format("GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT~n",[]),
	    false;
        ?GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT ->
	    io:format("GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT~n",[]),
	    false;
        ?GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT    ->
	    io:format("GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT~n",[]),
	    false;
        ?GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT ->
	    io:format("GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT~n",[]),
	    false;
        ?GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT ->
	    io:format("GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT~n",[]),
	    false
    end.

%%%%%%%%%%%%% Wrappers for functions that differs between wx and esdl

bindFramebuffer(W, Fbo) ->
    gl:bindFramebuffer(W,Fbo).

project(X,Y,Z, Mod, Proj, View) ->
    {_, RX,RY,RZ} = glu:project(X,Y,Z, mat(Mod), mat(Proj), View),
    {RX,RY,RZ}.

unProject(X,Y,Z, Mod, Proj, View) ->
    {_, RX,RY,RZ} = glu:unProject(X,Y,Z, mat(Mod), mat(Proj), View),
    {RX,RY,RZ}.

mat(Mat) when tuple_size(Mat) =:= 16 ->  Mat;
mat(Mat) when is_tuple(Mat) -> e3d_mat:expand(Mat);
mat(List) when is_list(List) ->  mat(list_to_tuple(List)).

triangulate(Normal, Pos0) ->
    {Tris0, BinPos} = glu:tesselate(Normal, Pos0),
    Tris = tris(Tris0),
    Res = {Tris, [{X,Y,Z}|| <<X:64/float-native,Y:64/float-native, Z:64/float-native>> <= BinPos]},
    %%io:format("~p~n~p~n~p~n",[Pos0, Tris, element(2, Res)]),
    Res.

tris([A,B,C|Rs]) ->
    [{A+1,B+1,C+1}|tris(Rs)];
tris([]) -> [].

deleteTextures(List) ->
    gl:deleteTextures(List).

deleteRenderbuffers(List) ->
    gl:deleteRenderbuffers(List).

deleteFramebuffers(List) ->
    gl:deleteFramebuffers(List).

%% This is a bug in wx it should take a list as argument
drawElements(O,L,T = ?GL_UNSIGNED_INT,What) when is_list(What) ->
    Bin = << <<Val:32/unsigned-native>> || Val <- What>>,
    gl:drawElements(O,L,T,Bin);
drawElements(O,L,T,What) ->
    gl:drawElements(O,L,T,What).

