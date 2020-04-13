%%
%%  wpc_thread --
%%
%%     Helicoidal and Non-Helicoidal Thread Plugin
%%
%%  Copyright (c) 2020 Micheus
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wpc_thread).
-export([init/0,menu/2,command/2]).
-include_lib("wings/src/wings.hrl").
-import(math, [cos/1,sin/1,pi/0]).

init() -> true.

menu({shape}, Menu) ->
    init_menu(Menu);
menu(_, Menu) -> Menu.

init_menu(Menu) ->
    case parse_menu(Menu,[]) of
        [] -> Menu ++ menu();
        Menu0 -> Menu0
    end.

menu() ->
    [{thread(),thread,?__(2,"Create a thread"),[option]}].

parse_menu([],_) -> [];
parse_menu([{_,spring,_}=Spiral|Rest],Acc) ->
    lists:reverse([Spiral|Acc]) ++ menu() ++ Rest;
parse_menu([Elem|Rest],Acc) ->
    parse_menu(Rest, [Elem|Acc]).


thread() ->
    ?__(1,"Thread").

command({shape,{thread, Ask}}, St) -> make_thread(Ask, St);
command(_, _) -> next.

%%% The rest are local functions.

%%%
%%% Thread
%%%

thread_dialog() ->
    Hook = fun(Var, Val, Sto) ->
            case Var of
                thread_type ->
                    wings_dialog:enable(direction, Val==helicoid, Sto);
                _ -> ok
            end
           end,

    [{label_column, [
        {?__(1,"Sections"), {text,8,[{key,sections},{range,{3,infinity}}]}},
        {" ", separator},
        {?__(2,"Top Radius"), {text,0.5,[{key,top_radius},{range,{0.0,infinity}}]}},
        {?__(3,"Bottom Radius"), {text,0.5,[{key,bottom_radius},{range,{0.0,infinity}}]}},
        {" ", separator},
        {?__(4,"Pitch"), {text,0.3,[{key,pitch},{range,{0.0001,infinity}}]}},
        {?__(5,"Crest Height"), {text,0.35,[{key,crest_h},{range,{0.0,infinity}}]}},
        {?__(6,"Crest Amount"), {text,5,[{key,occurences},{range,{1,infinity}}]}}]
     },
     {hradio, [
         {?__(10,"Helicoidal"),helicoid},
         {?__(11,"Non-helicoidal"),non_helicoid}],
      non_helicoid, [{key,thread_type},{title,?__(12,"Thread Type")},{hook,Hook}]},
     {hradio, [
         {?__(7,"Left hand"),left},
         {?__(8,"Right hand"),right}],
      right, [{key,direction},{title,?__(9,"Direction")}]},
     wings_shapes:transform_obj_dlg()].

make_thread(Arg, St) when is_atom(Arg) ->
    Qs = thread_dialog(),
    Label = ?__(1,"Thread Options"),
    wings_dialog:dialog_preview({shape,thread}, Arg, Label, Qs, St);
make_thread(Arg, _St) ->
    ArgDict = dict:from_list(Arg),
    Sections = dict:fetch(sections, ArgDict),
    Pitch = dict:fetch(pitch, ArgDict),
    TopRadius = dict:fetch(top_radius, ArgDict),
    BotRadius = dict:fetch(bottom_radius, ArgDict),
    CrestH = dict:fetch(crest_h, ArgDict),
    Occurences = dict:fetch(occurences, ArgDict),
    Dir = dict:fetch(direction, ArgDict),
    Type = dict:fetch(thread_type, ArgDict),
    Modify = [{dict:fetch(rot_x, ArgDict), dict:fetch(rot_y, ArgDict), dict:fetch(rot_z, ArgDict)},
              {dict:fetch(mov_x, ArgDict), dict:fetch(mov_y, ArgDict), dict:fetch(mov_z, ArgDict)},
              dict:fetch(ground, ArgDict)],

    Height = Pitch*(Occurences+0.5),
    Rows = Occurences+1,
    make_thread(Type, Dir, Sections, Height, TopRadius, BotRadius, CrestH, Pitch, Rows, Modify).

%%%
%%% Thread
%%%

make_thread(Type, Dir, Sections, Height, TopRadius, BotRadius, CrestH, Pitch, Rows, [Rot, Mov, Ground]) ->
    Vs1 = thread_verts(Type,Sections,TopRadius,BotRadius,CrestH,Pitch,Rows,Height),
    Fs0 = thread_faces(Type,Sections,Rows),
    {Vs0,Fs} = set_direction(Dir,Vs1,Fs0),
    Vs = wings_shapes:transform_obj(Rot,Mov,Ground,Vs0),
   {new_shape,thread(),Fs,Vs}.

thread_verts(Type, Sections, TopRadius, BotRadius, CrestH, Pitch, Rows, Height) ->
    RadRange = BotRadius-TopRadius,
    RadInc = RadRange/(Rows-1),
    Y = Height/2.0,
    PitchInc = Pitch/2.0,
    SubPitchInc = Pitch/Sections,
    Delta = pi()*2/Sections,
    Rings = lists:seq(Sections-1,0,-1),
    lists:foldr(fun(Idx, Acc) ->
                    YInc = Idx*Pitch,
                    RInc = Idx*RadInc,
                    Ring0 = ring_of_verts(Type, Rings, SubPitchInc, Delta, Y-YInc, TopRadius+RInc+CrestH),
                    Ring1 = ring_of_verts(Type, Rings, SubPitchInc, Delta, Y-(YInc+PitchInc), TopRadius+RInc),
                    Ring0++Ring1++Acc
                end, [], lists:seq(0,Rows-1)).

thread_faces(helicoid, N, Rows) ->
    R0 = (Rows-1)*2,
    R = (Rows*2-1)*N,
    Top = lists:reverse(lists:seq(N-1,0,-1)),
    Bottom = lists:seq(R+N-1,R, -1),
    Ns = lists:seq(0, N-2),
    Sides =
        lists:foldl(fun(Idx, Acc) ->
                        Sides = [[Idx*N + I, (Idx+1)*N + I,
                                  (Idx+1)*N + ((I+1) rem N), Idx*N + ((Idx*N+I+1) rem N)] || I <- Ns],
                        if Idx < (R0-1) ->
                            Stitch = [[Idx*N + N-1, (Idx+1)*N + N-1, (Idx+2)*N + N, (Idx+2)*N]];
                        Idx < R0 ->
                            Stitch = [[Idx*N + N-1, (Idx+1)*N + N-1, (Idx+2)*N]];
                        true ->
                            Stitch = [[Idx*N + N-1, (Idx+1)*N + N-1, (Idx+1)*N]]
                        end,
                        Acc++Sides++Stitch
                    end, [[0, N-1, N],[N-1, 2*N, N]], lists:seq(0,R0)),
    [Top, Bottom | Sides];
thread_faces(non_helicoid, N, Rows) ->
    R0 = (Rows-1)*2,
    R = (Rows*2-1)*N,
    Top = lists:reverse(lists:seq(N-1,0,-1)),
    Bottom = lists:seq(R+N-1,R, -1),
    Ns = lists:seq(0, N-1),
    Sides =
        lists:foldl(fun(Idx, Acc) ->
                        Sides = [[Idx*N + I, (Idx+1)*N + I,
                                  (Idx+1)*N + ((I+1) rem N), Idx*N + ((Idx*N+I+1) rem N)] || I <- Ns],
                        Acc++Sides
                    end, [], lists:seq(0,R0)),
    [Top, Bottom | Sides].

ring_of_verts(helicoid, Rings, YInc, Delta, YAxis, XZAxis) ->
    [{XZAxis*cos(I*Delta), YAxis+YInc*I, XZAxis*sin(I*Delta)} || I <- Rings];
ring_of_verts(non_helicoid, Rings, _, Delta, YAxis, XZAxis) ->
    [{XZAxis*cos(I*Delta), YAxis, XZAxis*sin(I*Delta)} || I <- Rings].

set_direction(left,Vs,Fs) -> {Vs,Fs};
set_direction(right,Vs0,Fs0) ->
    FlipX = e3d_mat:scale(-1.0, 1.0, 1.0),
    Vs = [e3d_mat:mul_point(FlipX, Pos) || Pos <- Vs0],
    Fs = [lists:reverse(F) || F <- Fs0],
    {Vs,Fs}.