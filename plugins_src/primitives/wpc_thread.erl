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
        {?__(6,"Crest Amount"), {text,5,[{key,occurrences},{range,{1,infinity}}]}}]
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
make_thread(Arg0, _St) ->
    Arg = maps:from_list(Arg0),
    Sections = maps:get(sections, Arg),
    Pitch = maps:get(pitch, Arg),
    TopRadius = maps:get(top_radius, Arg),
    BotRadius = maps:get(bottom_radius, Arg),
    CrestH = maps:get(crest_h, Arg),
    Occurrences = maps:get(occurrences, Arg),
    Dir = maps:get(direction, Arg),
    Type = maps:get(thread_type, Arg),
    Modify = [{maps:get(rot_x, Arg), maps:get(rot_y, Arg), maps:get(rot_z, Arg)},
              {maps:get(mov_x, Arg), maps:get(mov_y, Arg), maps:get(mov_z, Arg)},
              maps:get(ground, Arg)],

    Height = Pitch*(Occurrences+0.5),
    Rows = Occurrences+1,
    make_thread(Type, Dir, Sections, Height, TopRadius, BotRadius, CrestH, Pitch, Rows, Modify).

%%%
%%% Thread
%%%

make_thread(Type, Dir, Sections, Height, TopRadius, BotRadius, CrestH, Pitch, Rows, [Rot, Mov, Ground]) ->
    Vs1 = thread_verts(Type,Sections,TopRadius,BotRadius,CrestH,Pitch,Rows,Height),
    Fs0 = thread_faces(Type,Sections,Rows),
    {Vs0,Fs1} = set_direction(Dir,Vs1,Fs0),
    Fs = thread_uv_faces(Dir,Sections,Vs0,Fs1),
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

thread_faces(helicoid=Type, N, Rows) ->
    R0 = (Rows-1)*2,
    R = (Rows*2-1)*N,
    Top = lists:seq(0,N-1),
    Bottom = lists:seq(R+N-1,R, -1),
    Sides = build_sides(Type,R0,N),
    [Top, Bottom | Sides];
thread_faces(non_helicoid=Type, N, Rows) ->
    R0 = (Rows-1)*2,
    R = (Rows*2-1)*N,
    Top = lists:reverse(lists:seq(N-1,0,-1)),
    Bottom = lists:seq(R+N-1,R, -1),
    Sides = build_sides(Type,R0,N),
    [Top, Bottom | Sides].

build_sides(helicoid=Type, R, N) ->
    Ns = lists:seq(0, N-2),
    Fs = [[0, N-1, N],[N-1, 2*N, N]],
    build_sides(Type,0,R,N,Ns,Fs);
build_sides(non_helicoid=Type, R, N) ->
    Ns = lists:seq(0, N-1),
    build_sides(Type,0,R,N,Ns,[]).

build_sides(_, Idx, R, _, _, Acc) when Idx > R ->
    Acc;
build_sides(helicoid=Type, Idx, R, N, Ns, Acc) ->
    Sides = [[Idx*N + I, (Idx+1)*N + I,
              (Idx+1)*N + ((I+1) rem N), Idx*N + ((Idx*N+I+1) rem N)] || I <- Ns],
    if Idx < (R-1) ->
        Stitch = [[Idx*N + N-1, (Idx+1)*N + N-1, (Idx+2)*N + N, (Idx+2)*N]];
        Idx < R ->
            Stitch = [[Idx*N + N-1, (Idx+1)*N + N-1, (Idx+2)*N]];
        true ->
            Stitch = [[Idx*N + N-1, (Idx+1)*N + N-1, (Idx+1)*N]]
    end,
    build_sides(Type,Idx+1,R,N,Ns,Acc++Sides++Stitch);
build_sides(non_helicoid=Type, Idx, R, N, Ns, Acc) ->
    Sides = [[Idx*N + I, (Idx+1)*N + I,
              (Idx+1)*N + ((I+1) rem N), Idx*N + ((Idx*N+I+1) rem N)] || I <- Ns],
    build_sides(Type,Idx+1,R,N,Ns,Acc++Sides).

ring_of_verts(helicoid, Rings, YInc, Delta, YAxis, XZAxis) ->
    [{XZAxis*cos(I*Delta), YAxis+YInc*I, XZAxis*sin(I*Delta)} || I <- Rings];
ring_of_verts(non_helicoid, Rings, _, Delta, YAxis, XZAxis) ->
    [{XZAxis*cos(I*Delta), YAxis, XZAxis*sin(I*Delta)} || I <- Rings].

set_direction(left,Vs,Fs) ->
    {Vs,Fs};
set_direction(right,Vs0,Fs0) ->
    FlipX = e3d_mat:scale(-1.0, 1.0, 1.0),
    Vs = [e3d_mat:mul_point(FlipX, Pos) || Pos <- Vs0],
    Fs = [lists:reverse(F) || F <- Fs0],
    {Vs,Fs}.
%%%
%%% UV layout: the sides are a strip in the upper three quarters (U from the
%%% angle of the section, V from the height), the two end caps are polygons
%%% in the lower quarter.
%%%

thread_uv_faces(Dir, N, Vs, [Top,Bottom|Sides]) ->
    Ys = [Y || {_,Y,_} <- Vs],
    YMin = lists:min(Ys),
    YDiff = max(lists:max(Ys)-YMin, 1.0e-9),
    Vt = list_to_tuple(Vs),
    Ctx = {Dir,N,Vt,YMin,YDiff},
    Factor = thread_cap_factor(Top, Vt, YDiff),
    RTop = cap_radius(Top, Vt, Factor),
    RBottom = cap_radius(Bottom, Vt, Factor),
    Shrink = min(1.0, 0.12/max(RTop, RBottom)),
    [thread_cap(Top, {0.125,0.125}, RTop*Shrink),
     thread_cap(Bottom, {0.375,0.125}, RBottom*Shrink) |
     [thread_side(F, Ctx) || F <- Sides]].

%% The sides map the perimeter of the top to U and the height to V, so the
%% caps use the geometric mean of both scales to keep the texture scale; if
%% they do not fit in their band both are reduced by the same amount.
thread_cap_factor(Top, Vt, YDiff) ->
    Ps = [element(V+1, Vt) || V <- Top],
    Pairs = lists:zip(Ps, tl(Ps) ++ [hd(Ps)]),
    Lens = [e3d_vec:dist(A, B) || {A,B} <- Pairs],
    Perimeter = lists:sum(Lens),
    (1.0/Perimeter) * (0.75/YDiff).

thread_cap(Idx, {Cx,Cy}, R) ->
    N = length(Idx),
    Delta = pi()*2/N,
    UVs = [{Cx+R*cos(I*Delta), Cy+R*sin(I*Delta)} || I <- lists:seq(0, N-1)],
    {default,Idx,UVs,none,none}.

cap_radius(Idx, Vt, Factor) ->
    Ps = [element(V+1, Vt) || V <- Idx],
    N = length(Ps),
    Pairs = lists:zip(Ps, tl(Ps) ++ [hd(Ps)]),
    Sum = lists:foldl(fun({A,B}, Acc) ->
                          e3d_vec:add(Acc, e3d_vec:cross(A, B))
                      end, {0.0,0.0,0.0}, Pairs),
    Area = e3d_vec:len(Sum) / 2,
    RegArea = (N/2) * math:sin(2*math:pi()/N),
    math:sqrt(Area*Factor/RegArea).

thread_side(F, {_,N,_,_,_}=Ctx) ->
    Ss = [V rem N || V <- F],
    Seam = lists:member(0, Ss) andalso lists:member(N-1, Ss),
    {default,F,[thread_uv(V, Seam, Ctx) || V <- F],none,none}.

thread_uv(V, Seam, {Dir,N,VsT,YMin,YDiff}) ->
    S = V rem N,
    U0 = case Seam andalso (S =:= N-1) of
             true -> 1.0;
             false -> (N-1-S)/N
         end,
    U = case Dir of
            left -> 1.0-U0;
            right -> U0
        end,
    {_,Y,_} = element(V+1, VsT),
    {U, 0.25+0.75*(Y-YMin)/YDiff}.
