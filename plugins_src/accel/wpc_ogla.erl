%%
%%  wpc_ogla.erl --
%%
%%     Plug-in for accelerating certain OpenGL operations.
%%
%%  Copyright (c) 2001-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_ogla.erl,v 1.5 2004/12/30 09:35:00 bjorng Exp $
%%

-module(wpc_ogla).
-export([init/0]).
-export([two/2,tri/3,quad_tri/4,quad/4,triangulate/2]).

-define(FL32, :32/native-float).

-import(lists, [reverse/1]).

init() ->
    Dir = filename:dirname(code:which(?MODULE)),
    Name = "wings_ogla_drv",
    case erl_ddll:load_driver(Dir, Name) of
	ok -> ok;
	{error,Reason} ->
	    io:format("Failed to load ~s in ~s\n~s\n",
		      [Name,Dir,erl_ddll:format_error(Reason)]),
	    erlang:fault(startup_fault)
    end,
    case open_port({spawn,Name},[]) of
	Port when is_port(Port) ->
	    register(wings_ogla_port, Port);
	_ ->
	    io:format("Failed to open port ~s\n", [Name]),
	    erlang:fault(startup_fault)
    end,

    false.

tri({Ax,Ay,Az}, {Bx,By,Bz}, {Cx,Cy,Cz}) ->
    %%  gl:vertex3fv(A),
    %%  gl:vertex3fv(B),
    %%  gl:vertex3fv(C);
    Bin = <<Ax?FL32,Ay?FL32,Az?FL32,
	   Bx?FL32,By?FL32,Bz?FL32,
	   Cx?FL32,Cy?FL32,Cz?FL32>>,
    erlang:port_control(wings_ogla_port, 0, Bin).

quad_tri({Ax,Ay,Az}, {Bx,By,Bz}, {Cx,Cy,Cz}, {Dx,Dy,Dz}) ->
    %%  gl:vertex3fv(A),
    %%  gl:vertex3fv(B),
    %%  gl:vertex3fv(C),
    %%  gl:vertex3fv(C),
    %%  gl:vertex3fv(D),
    %%  gl:vertex3fv(A)
    Bin = <<Ax?FL32,Ay?FL32,Az?FL32,
	   Bx?FL32,By?FL32,Bz?FL32,
	   Cx?FL32,Cy?FL32,Cz?FL32,
	   Dx?FL32,Dy?FL32,Dz?FL32>>,
    erlang:port_control(wings_ogla_port, 1, Bin).

quad({Ax,Ay,Az}, {Bx,By,Bz}, {Cx,Cy,Cz}, {Dx,Dy,Dz}) ->
    %%  gl:vertex3fv(A),
    %%  gl:vertex3fv(B),
    %%  gl:vertex3fv(C),
    %%  gl:vertex3fv(D)
    Bin = <<Ax?FL32,Ay?FL32,Az?FL32,
	   Bx?FL32,By?FL32,Bz?FL32,
	   Cx?FL32,Cy?FL32,Cz?FL32,
	   Dx?FL32,Dy?FL32,Dz?FL32>>,
    erlang:port_control(wings_ogla_port, 2, Bin).

two({Ax,Ay,Az}, {Bx,By,Bz}) ->
    %%  gl:vertex3fv(A),
    %%  gl:vertex3fv(B)
    Bin = <<Ax?FL32,Ay?FL32,Az?FL32,
	   Bx?FL32,By?FL32,Bz?FL32>>,
    erlang:port_control(wings_ogla_port, 3, Bin).

triangulate(N, Ps) ->
    %% Vs = lists:seq(0, length(Ps)-1),
    %% Fs0 = e3d_mesh:triangulate_face(#e3d_face{vs=Vs}, N, Ps),
    %% [{A+1,B+1,C+1} || #e3d_face{vs=[A,B,C]} <- Fs0].
    case N of
	{0.0,0.0,0.0} ->
	    %% Undefined normal - something is seriously wrong
	    %% with this polygon. Return a fake triangulation.
	    {[{1,2,3}],Ps};
	_ ->
	    Bin = vs_to_bin([N|Ps], []),
	    BinRes = erlang:port_control(wings_ogla_port, 4, Bin),
	    {Tris,MorePs} = triangulate_1(BinRes, []),
	    {Tris,Ps++MorePs}
    end.
    
triangulate_1(<<0:32/native,T/binary>>, Acc) ->
    {reverse(Acc),triangulate_2(T, [])};
triangulate_1(<<Va:32/native,Vb:32/native,Vc:32/native,T/binary>>, Acc) ->
    triangulate_1(T, [{Va,Vb,Vc}|Acc]).

triangulate_2(<<X:64/native-float,Y:64/native-float,Z:64/native-float,
	       T/binary>>, Acc) ->
    triangulate_2(T, [{X,Y,Z}|Acc]);
triangulate_2(<<>>, Acc) ->
    reverse(Acc).

vs_to_bin([{X,Y,Z}|Vs], Acc) ->
    vs_to_bin(Vs, [<<X:64/native-float,Y:64/native-float,Z:64/native-float>>|Acc]);
vs_to_bin([], Acc) -> reverse(Acc).
