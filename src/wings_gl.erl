%%
%%  wings_gl.erl --
%%
%%     A few OpenGL utilities.
%%
%%  Copyright (c) 2001-2008 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_gl).
-export([init_extensions/0,is_ext/1,is_ext/2,
	 init_restrictions/0,is_restriction/1,
	 error_string/1]).

%% GLSL exports
-export([support_shaders/0,
	 uloc/2, set_uloc/3,
	 compile/2,link_prog/1]).

%% FBO exports
-export([have_fbo/0, setup_fbo/2, delete_fbo/1]).

%% GL wrappers
-export([callLists/1, project/6, unProject/6, triangulate/2, deleteTextures/1]).

%% Debugging.
-export([check_error/2]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

-ifdef(USE_WX).
-define(genFramebuffers,genFramebuffers).
-define(bindFramebuffer, bindFramebuffer).
-define(framebufferTexture2D,framebufferTexture2D).
-define(genRenderbuffers, genRenderbuffers).
-define(bindRenderbuffer,bindRenderbuffer).
-define(renderbufferStorage,renderbufferStorage).
-define(framebufferRenderbuffer,framebufferRenderbuffer).
-define(checkFramebufferStatus, checkFramebufferStatus).
-else.
-define(genFramebuffers,genFramebuffersEXT).
-define(bindFramebuffer, bindFramebufferEXT).
-define(framebufferTexture2D,framebufferTexture2DEXT).
-define(genRenderbuffers, genRenderbuffersEXT).
-define(bindRenderbuffer,bindRenderbufferEXT).
-define(renderbufferStorage,renderbufferStorageEXT).
-define(framebufferRenderbuffer,framebufferRenderbufferEXT).
-define(checkFramebufferStatus, checkFramebufferStatusEXT).
-endif.

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
	    erlang:error(gl_error, [Mod,Line])
    end.
-else.
check_error(_Mod, _Line) ->
    ok.
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Shader compilation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Use ARB functions until Mac OsX have been upgraded to opengl 2.0

support_shaders() ->
    is_ext(['GL_ARB_fragment_shader', 'GL_ARB_vertex_shader']). 

uloc(Prog, What) ->
    try gl:getUniformLocation(Prog, What) of
	-1 -> 
	    io:format("Warning the uniform variable '~p' is not used~n",[What]),
	    -1;
	Where -> 
	    Where
    catch _:_ -> 
	    ErrStr = "ERROR The uniform variable ~p should be a string~n",
	    Err = io_lib:format(ErrStr, [What]),
	    throw(lists:flatten(Err))
    end.

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
    gl:uniform4f(Pos,A,B,C,D).

compile(vertex, Bin) when is_binary(Bin) ->
    compile2(?GL_VERTEX_SHADER, "Vertex", Bin);
compile(fragment, Bin) when is_binary(Bin) ->
    compile2(?GL_FRAGMENT_SHADER, "Fragment", Bin).

compile2(Type,Str,Src) ->
    Handle = gl:createShaderObjectARB(Type),    
    ok = shaderSource(Handle, [Src]),
    ok = gl:compileShader(Handle),
    check_status(Handle,Str, ?GL_OBJECT_COMPILE_STATUS_ARB),
    Handle.

link_prog(Objs) when is_list(Objs) ->    
    Prog = gl:createProgramObjectARB(),
    [gl:attachObjectARB(Prog,ObjCode) || ObjCode <- Objs],
    [gl:deleteShader(ObjCode) || ObjCode <- Objs],
    gl:linkProgram(Prog),
    check_status(Prog,"Link result", ?GL_OBJECT_LINK_STATUS_ARB),
    Prog.

check_status(Handle,Str, What) ->
    case gl:getObjectParameterivARB(Handle, What) of
	1 -> 
	    %% printInfo(Handle,Str), %% Check status even if ok
	    Handle;
	_E -> 	    
	    printInfo(Handle,Str),
	    throw("Compilation failed")
    end.

printInfo(ShaderObj,Str) ->
    Len = gl:getObjectParameterivARB(ShaderObj, ?GL_OBJECT_INFO_LOG_LENGTH_ARB),
    case Len > 0 of
	true ->
	    case catch gl:getInfoLogARB(ShaderObj, Len) of
		{_, []} -> 
		    ok;
		{_, InfoStr} ->
		    io:format("Info: ~s:~n ~s ~n", [Str,InfoStr]),
		    case string:str(InfoStr, "oftware") of
			0 -> ok;
			_ -> 
			    throw("Shader disabled would run in Software mode")
		    end;
		Error ->
		    io:format("Internal error PrintInfo crashed with ~p ~n", 
			      [Error])
	    end;
	false ->
	    ok
    end.

%%%%%%%%%%%%%  Framebuffer object %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

have_fbo() ->
    is_ext('GL_EXT_framebuffer_object').

%% Size = {W,H}
%% What = {BufferType, Options}
%%   => [{BufferType, Identifer}] | not_supported
setup_fbo(Size, What) ->
    case have_fbo() of
	false -> not_supported;
	true -> setup_fbo_1(Size, What)
    end.

setup_fbo_1(Size, Types) ->
    [FB] = gl:?genFramebuffers(1),
    gl:?bindFramebuffer(?GL_FRAMEBUFFER_EXT, FB),
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
	    gl:generateMipmapEXT(?GL_TEXTURE_2D);
       true -> ok
    end,

    gl:?framebufferTexture2D(?GL_FRAMEBUFFER_EXT,
			     ?GL_COLOR_ATTACHMENT0_EXT + Count,
			     ?GL_TEXTURE_2D, Col, 0),
    {color, Col};
setup_fbo_2({depth, Options}, {W,H}, _) ->
    [Depth] = gl:?genRenderbuffers(1),
    %% Init depth texture
    gl:?bindRenderbuffer(?GL_RENDERBUFFER_EXT, Depth),
    Internal = proplists:get_value(internal, Options, ?GL_DEPTH_COMPONENT24),
    gl:?renderbufferStorage(?GL_RENDERBUFFER_EXT,Internal, W, H),
    gl:?framebufferRenderbuffer(?GL_FRAMEBUFFER_EXT,
				?GL_DEPTH_ATTACHMENT_EXT,
				?GL_RENDERBUFFER_EXT, Depth),
    {depth, Depth}.

delete_fbo(List) ->
    gl:?framebufferTexture2D(?GL_FRAMEBUFFER_EXT,
			     ?GL_COLOR_ATTACHMENT0_EXT,
			     ?GL_TEXTURE_2D,0,0),
    Textures = [Col || {color, Col} <- List],
    deleteTextures(Textures),
    gl:?framebufferRenderbuffer(?GL_FRAMEBUFFER_EXT,
				?GL_DEPTH_ATTACHMENT_EXT,
				?GL_RENDERBUFFER_EXT, 0),
    Depth = [D || {depth, D} <- List],
    deleteRenderbuffers(Depth),
    gl:?bindFramebuffer(?GL_FRAMEBUFFER_EXT, 0),
    FB = [F || {framebuffer, F} <- List],
    deleteFramebuffers(FB).

check_fbo_status(FB) ->
    case gl:?checkFramebufferStatus(?GL_FRAMEBUFFER_EXT) of
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

-ifdef(USE_WX).
callLists(List) ->  gl:callLists(List).

project(X,Y,Z, Mod, Proj, View) ->
    {_, RX,RY,RZ} = glu:project(X,Y,Z, list_to_tuple(Mod), list_to_tuple(Proj), View),
    {RX,RY,RZ}.

unProject(X,Y,Z, Mod, Proj, View) ->
    {_, RX,RY,RZ} = glu:unProject(X,Y,Z, list_to_tuple(Mod), list_to_tuple(Proj), View),
    {RX,RY,RZ}.

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

shaderSource(Handle, Src) ->
    gl:shaderSource(Handle, Src).

-else.
callLists(List) ->  gl:callLists(length(List), ?GL_UNSIGNED_INT, List).

project(X,Y,Z, Mod, Proj, View) ->
    glu:project(X,Y,Z, Mod, Proj, View).

unProject(X,Y,Z, Mod, Proj, View) ->
    glu:unProject(X,Y,Z, Mod, Proj, View).

triangulate(Normal, Pos) ->
    glu:triangulate(Normal,Pos).

deleteTextures(List) ->
    gl:deleteTextures(length(List), List).

deleteRenderbuffers(List) ->
    gl:deleteRenderbuffersEXT(length(List),List).

deleteFramebuffers(List) ->
    gl:deleteFramebuffersEXT(length(List),List).

shaderSource(Handle, Src) ->
    ok = gl:shaderSource(Handle, 1, Src, [-1]).

-endif.
