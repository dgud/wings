%%
%%  wings_gl.erl --
%%
%%     A few OpenGL utilities.
%%
%%  Copyright (c) 2001-2005 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_gl.erl,v 1.5 2006/04/27 13:46:54 dgud Exp $
%%

-module(wings_gl).
-export([init_extensions/0,is_ext/1,is_ext/2,
	 init_restrictions/0,is_restriction/1,
	 error_string/1]).

%% GLSL exports
-export([support_shaders/0,
	 uloc/2, set_uloc/3,
	 compile/2,link_prog/1]).

%% Debugging.
-export([check_error/2]).

-define(NEED_OPENGL, 1).
-include("wings.hrl").

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
is_ext(Wanted) when is_tuple(Wanted), size(Wanted) >= 2 ->
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
    gl:uniform3f(Pos,A,B,C).

compile(vertex, Bin) when is_binary(Bin) ->
    compile2(?GL_VERTEX_SHADER, "Vertex", Bin);
compile(fragment, Bin) when is_binary(Bin) ->
    compile2(?GL_FRAGMENT_SHADER, "Fragment", Bin).

compile2(Type,Str,Src) ->
    Handle = gl:createShaderObjectARB(Type),    
    ok = gl:shaderSource(Handle, 1, [Src], [-1]),
    ok = gl:compileShader(Handle),
    check_status(Handle,Str, ?GL_OBJECT_COMPILE_STATUS),
    Handle.

link_prog(Objs) when is_list(Objs) ->    
    Prog = gl:createProgramObjectARB(),
    [gl:attachObjectARB(Prog,ObjCode) || ObjCode <- Objs],
    [gl:deleteShader(ObjCode) || ObjCode <- Objs],
    gl:linkProgram(Prog),
    check_status(Prog,"Link result", ?GL_OBJECT_LINK_STATUS),
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
    Len = gl:getObjectParameterivARB(ShaderObj, ?GL_OBJECT_INFO_LOG_LENGTH),
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
