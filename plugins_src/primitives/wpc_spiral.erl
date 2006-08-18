%% Spiral Plugin for Wings 0.90 or higher.
%%
%% By Jakob Cederlund (jakobc@users.sourceforge.net).
%% Modifications by Bjorn Gustavsson (bjorng@users.sourceforge.net).
%%

-module(wpc_spiral).
-export([init/0,menu/2,command/2]).

-import(math, [sqrt/1,cos/1,sin/1,pi/0]).
-include("wings_intl.hrl").

init() ->
    true.

menu({shape,more}, []) ->
    spiral_menu();
menu({shape,more}, Menu) ->
    Menu ++ [separator|spiral_menu()];
menu(_, Menu) -> Menu.

spiral_menu() ->
    [{?__(1,"Spiral"),spiral,[option]},
     {?__(2,"Spring"),spring,[option]}].

command({shape,{more,{spiral,Ask}}}, _St) -> make_spiral(Ask);
command({shape,{more,{spring,Ask}}}, _St) -> make_spring(Ask);
command(_, _) -> next.

%%% The rest are local functions.

make_spiral(Ask) when is_atom(Ask) ->
    wpa:ask(Ask, ?__(1,"Create Spiral"),
	    [{?__(2,"Loops"),2,[{range,{1,32}}]},
	     {?__(3,"Segments"),16,[{range,{3,128}}]},
	     {?__(4,"Sections"),8,[{range,{2,64}}]}],
	    fun(Res) -> {shape,{more,{spiral,Res}}} end);
make_spiral([L,Ns,Nl]) ->
    Vs = spiral_vertices(Ns, Nl, L),
    Fs = spiral_faces(Ns, Nl, L),
    {new_shape,"spiral",Fs,Vs}.
 
make_spring(Ask) when is_atom(Ask) ->
    wpa:ask(Ask, ?__(1,"Create Spring"),
	    [{?__(2,"Loops"),2,[{range,{1,32}}]},
	     {?__(3,"Segments"),16,[{range,{3,128}}]},
	     {?__(4,"Sections"),8,[{range,{2,64}}]}],
	    fun(Res) -> {shape,{more,{spring,Res}}} end);
make_spring([L,Ns,Nl]) ->
    Vs = spiral_vertices2(Ns, Nl, L),
    Fs = spiral_faces(Ns, Nl, L),
    {new_shape,"spring",Fs,Vs}.
    
spiral_faces(Ns0, Nl, L) ->
    Nl2= Nl*2,
    Ns = Ns0*L,
    Slices= [ [ [(I+1) rem Ns + J*Ns, I + J*Ns,
		 I+ ((J+1) rem Nl2) *Ns, (I+1) rem Ns + ((J+1) rem Nl2)*Ns]
		|| I <- lists:seq(0, Ns - 2)]
	      || J <- lists:seq(0, Nl2 - 1)],
    F0 = lists:append(Slices),
    F1 = [lists:seq((Nl2-1)*Ns, 0, -Ns) | F0],
    F = [lists:seq(Ns-1, Ns*Nl2-1, Ns) | F1],
    F. 

spiral_circle(N, Y, R, D, L) ->
    Delta= pi()*2 / N,
    [{(R+I*D)*cos(I*Delta), Y, (R+I*D)*sin(I*Delta)} ||
	I <- lists:seq(0, L*N-1)].

spiral_vertices(Ns, Nl, L) ->
    Nl2 = Nl*2,
    Delta= 2*pi() / Nl2,
    PosAndRads= [{0.25*sin(I*Delta), 0.75 + 0.25*cos(I*Delta)} ||
		    I <- lists:seq(0, Nl2 - 1)],
    Circles= [spiral_circle(Ns, Pos, Rad, 0.05, L) ||
		 {Pos, Rad} <- PosAndRads],
    lists:flatten(Circles).

spiral_circle2(N, Y, R, D, L) ->
    Delta= pi()*2 / N,
    [{R*cos(I*Delta), Y+I*D, R*sin(I*Delta)} || I <- lists:seq(0, L*N-1)].

spiral_vertices2(Ns, Nl, L) ->
    Nl2 = Nl*2,
    Delta= 2*pi() / Nl2,
    PosAndRads= [{0.25*sin(I*Delta), 0.75 + 0.25*cos(I*Delta)} ||
		    I <- lists:seq(0, Nl2 - 1)],
    Circles= [spiral_circle2(Ns, Pos, Rad, 0.05, L) ||
		 {Pos, Rad} <- PosAndRads],
    lists:flatten(Circles).
