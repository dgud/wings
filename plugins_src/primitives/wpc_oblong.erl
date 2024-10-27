%%
%%  wpc_oblong --
%%
%%     Oblong and Oblong void
%%
%%  Copyright (c) 2023 Micheus
%%
%%  Inspired on the 3dsMax plugin by Christophe Blattmann:
%%      https://www.christopheblattmann.com/oblong
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wpc_oblong).
-export([init/0,menu/2,command/2]).
-include_lib("wings/src/wings.hrl").


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
    [{oblong(),oblong,?__(2,"Create an oblong"),[option]}].

parse_menu([],_) -> [];
parse_menu([{_,thread,_}=Thread|Rest],Acc) ->
    lists:reverse([Thread|Acc]) ++ menu() ++ Rest;
parse_menu([Elem|Rest],Acc) ->
    parse_menu(Rest, [Elem|Acc]).


oblong() ->
    ?__(1,"Oblong").

command({shape,{oblong, Ask}}, St) -> make_oblong(Ask, St);
command(_, _) -> next.


%%%
%%% Oblong
%%%

oblong_dialog() ->
    Hook = fun(Var, Val, Sto0) ->
        case Var of
            length ->
                Width = wings_dialog:get_value(width, Sto0),
                if Val < (Width) ->
                    wings_dialog:set_value(length,Width,Sto0);
                true -> ok
                end;
            width ->
                Length = wings_dialog:get_value(length, Sto0),
                if Val > Length ->
                    wings_dialog:set_value(length,Val+0.1,Sto0);
                true -> ok
                end;
            bevel ->
                Type = wings_dialog:get_value(oblong_type, Sto0),
                Height = wings_dialog:get_value(height, Sto0),
                if Type == regular ->
                    Width = wings_dialog:get_value(width, Sto0);
                true ->
                    Width = wings_dialog:get_value(thickness, Sto0)
                end,
                Max = min(Height,Width),
                if Val > (Max/2.0) ->
                    wings_dialog:set_value(bevel,Max/2.0,Sto0);
                true -> ok
                end,
                wings_dialog:enable(b_sections,Val > 0.0,Sto0);
            oblong_type ->
                wings_dialog:enable([thickness,t_sections],Val==void,Sto0);
            thickness ->
                Width = wings_dialog:get_value(width, Sto0),
                if Val > (Width/2.0) ->
                    wings_dialog:set_value(thickness,Width/2.0,Sto0);
                true -> ok
                end;
            _ -> ok
        end
    end,

    [{label_column, [
        {?__(1,"Width"), {text,2.0,[{key,width},{range,{0.01,infinity}},{hook,Hook}]}},
        {?__(2,"Length"), {text,3.0,[{key,length},{range,{0.005,infinity}},{hook,Hook}]}},
        {?__(3,"Height"), {text,0.5,[{key,height},{range,{0.01,infinity}}]}},
        {?__(4,"Bevel"), {text,0.0,[{key,bevel},{range,{0.0,infinity}},{hook,Hook}]}},
        {" ", separator},
        {?__(5,"Side Sections"), {text,4,[{key,s_sections},{range,{2,infinity}}]}},
        {?__(6,"Height Slices"), {text,0,[{key,h_sections},{range,{0,infinity}}]}},
        {?__(7,"Bevel Slices"), {text,0,[{key,b_sections},{range,{0,infinity}}]}}]},
    {hradio, [{?__(8,"Regular"),regular}, {?__(9,"Void"),void}],
        regular, [{key,oblong_type},{title,?__(10,"Oblong Type")},{hook,Hook}]},
    {label_column,[
        {?__(11,"Thickness"), {text,0.8,[{key,thickness},{range,{0.0,infinity}},{hook,Hook}]}},
        {?__(12,"Thickness Slices"), {text,0,[{key,t_sections},{range,{0,infinity}}]}}]},
     wings_shapes:transform_obj_dlg()].

make_oblong(Arg, St) when is_atom(Arg) ->
    Qs = oblong_dialog(),
    Label = ?__(1,"Oblong Options"),
    wings_dialog:dialog_preview({shape,oblong}, Arg, Label, Qs, St);
make_oblong(Arg, _St) ->
    ArgDict = dict:from_list(Arg),
    Width = dict:fetch(width, ArgDict),
    Middle = (dict:fetch(length, ArgDict) - Width),
    Height = dict:fetch(height, ArgDict),
    Bevel = dict:fetch(bevel, ArgDict),
    SideSec = dict:fetch(s_sections, ArgDict),
    HeightSec = dict:fetch(h_sections, ArgDict),
    BevelSec = dict:fetch(b_sections, ArgDict),
    Type = dict:fetch(oblong_type, ArgDict),
    ThickSec = dict:fetch(t_sections, ArgDict),
    Thickness = dict:fetch(thickness, ArgDict),

    Modify = [{dict:fetch(rot_x, ArgDict), dict:fetch(rot_y, ArgDict), dict:fetch(rot_z, ArgDict)},
          {dict:fetch(mov_x, ArgDict), dict:fetch(mov_y, ArgDict), dict:fetch(mov_z, ArgDict)},
          dict:fetch(ground, ArgDict)],

    case Type of
        regular ->
            make_regular(Width,Middle,Height,Bevel,SideSec,HeightSec,ThickSec,BevelSec,Modify);
        void ->
            Min = (Width/2)-?EPSILON,
            Thickness1 = min(Min, Thickness),
            Bevel1 = min(Min-(Thickness1/2.0),Bevel),
            make_void(Width,Middle,Height,Bevel1,SideSec,HeightSec,ThickSec,BevelSec,Thickness1,Modify)
    end.

%%%
%%%  Oblong - regular
%%%
make_regular(Width, Middle, Height, Bevel, SideSec,
            HeightSec, _ThickSec, BevelSec, [Rot, Mov, Ground]) ->
    Vs2 = make_outer_contour(Width, Height, Bevel, HeightSec, BevelSec),
    Vs1 = calc_vertices(SideSec, Width, Middle, Vs2),
    Fs = calc_faces(regular,Vs1),
    Vs = wings_shapes:transform_obj(Rot,Mov,Ground,lists:flatten(Vs1)),
    {new_shape,oblong(),Fs,Vs}.

%%%
%%%  Oblong - void
%%%
make_void(Width, Middle, Height, Bevel, SideSec,
          HeightSec, ThickSec, BevelSec, Thickness, [Rot, Mov, Ground]) ->

    Vs3 = make_outer_contour(Width, Height, Bevel, HeightSec, BevelSec),
    Vs2 = calc_inner_contour(Width, Thickness, ThickSec, Vs3),
    Vs1 = calc_vertices(SideSec, Width, Middle, Vs2),
    Fs = calc_faces(void,Vs1),
    Vs = wings_shapes:transform_obj(Rot,Mov,Ground,lists:flatten(Vs1)),
    {new_shape,oblong(),Fs,Vs}.

%%%
%%%  Common functions
%%%

%%% Computes the outside contour that will be replicated to shape the object
make_outer_contour(Width, Height0, Bevel0, HeightSec, BevelSec) ->
    YTop = Height0/2.0,
    ZRig = Width/2.0,
    if Bevel0 > ?EPSILON ->
        if BevelSec > 0 ->
            Delta = (math:pi()/2.0)/(BevelSec+1);
            true ->
                Delta = math:pi()/2.0
        end,
        Bevel = min(Bevel0, min(YTop,ZRig)),
        Height = Height0-(Bevel*2.0),
        YSeg = Height/(HeightSec+1),
        BVs = [{0.0,
                (YTop-Bevel)+(Bevel*math:sin(I*Delta)),
                ZRig-Bevel+(Bevel*math:cos(I*Delta))} || I <- lists:seq(0,BevelSec+1)],
        Vs = lists:foldl(fun(_I, [{_,Y,Z}|_]=Acc) ->
            [{0.0,Y-YSeg,Z}|Acc]
                         end, BVs, lists:seq(trunc((HeightSec+1)/2),1,-1));
        true ->
            YSeg = Height0/(HeightSec+1),
            Vs = lists:foldl(fun(_I, [{+0.0,Y,Z}|_]=Acc) ->
                                     [{+0.0,Y-YSeg,Z}|Acc]
                             end, [{+0.0,YTop,ZRig}], lists:seq(1,trunc((HeightSec+1)/2)))
    end,
    if (HeightSec rem 2) == 0 -> Vs1 = Vs;
        true ->
            [_|Vs1] = Vs
    end,
    %% flipping vertically the upper half
    lists:reverse(Vs)++[{X,-Y,Z} || {X,Y,Z} <- Vs1].

%%% Computes the inside contour for a void object that will be appended
%%% to the outside contour to create the hole when replicated
calc_inner_contour(Width, Thickness0, ThickSec, [{XMin,YMin,ZMax}|_]=Vs2) ->
    Offset = (Width-Thickness0),
    %% mirroring the outside contour and adjusting the thickness
    [{_XMin,_YMax,ZMin}|_] = Vs1 = [{X,Y,-Z+Offset} || {X,Y,Z} <- lists:reverse(Vs2)],
    Thickness = (ZMax-ZMin),
    if Thickness > ?EPSILON ->
        ZSeg = Thickness/(ThickSec+1),
        ZBot = [{XMin,-YMin,ZMax-(I*ZSeg)} || I <- lists:seq(1,ThickSec)],
        ZTop = [{X,-Y,Z} || {X,Y,Z} <- lists:reverse(ZBot)];
    true ->
        ZBot = [],
        ZTop = []
    end,
    lists:flatten([Vs2,ZBot,Vs1|ZTop]).

%%% Computes the vertices for each segment/session using the contour
calc_vertices(SideSec, Width, Middle, Vs1) ->
    Delta = 180.0/SideSec,
    Arc = lists:seq(0, SideSec),
    ZRig = Width/2.0,
    Offset = (Middle-Width)/2,
    Fr = fun(Ang) ->
            M = e3d_mat:rotate(Ang,wings_util:make_vector(y)),
            Vs = [e3d_mat:mul_point(M,V) || V <- Vs1],
            [{Offset+ZRig+X, Y, Z} || {X,Y,Z} <- Vs]
         end,
    Vss = [Fr(I*Delta) || I <- Arc],
    Left = [[{X,Y,-Z} || {X,Y,Z} <- Vs] || Vs <- Vss],
    if Middle == 0.0 ->
        %% avoiding overlapping sections for Length equal to Width
        [_|Tmp0] = Left,
        [_|Tmp] = lists:reverse(Tmp0);
    true ->
        Tmp = lists:reverse(Left)
    end,
    Right = [[{-X,Y,Z} || {X,Y,Z} <- Vs] || Vs <- Tmp],
    Left ++ Right.

calc_faces(Type, [Vs|_]=Vss) ->
    NSecs = length(Vss),
    NVs = length(Vs),
    NTot = NSecs*NVs,
    Sides = [[[(I*NVs)+N, ((I+1)*NVs+N) rem NTot,
            (((I+1)*NVs)+N+1) rem NTot, (I*NVs)+N+1] || N <- lists:seq(0,NVs-2)] ||
                I <- lists:seq(0,NSecs-1)],
    if (Type == regular) ->
        UpLF = lists:seq((NSecs-1)*NVs,0,-NVs),
        DownLF = [V+(NVs-1)|| V <- lists:reverse(UpLF)],
        Caps = [UpLF, DownLF];
    true ->
        Caps = [[I*NVs, ((I+1)*NVs)-1,
                (((I+2)*NVs-1) rem NTot), (((I+1)*NVs) rem NTot)] || I <- lists:seq(0,NSecs-1)]
    end,
    lists:foldr(fun(Item, Acc) ->
                    Item++Acc
                end, Caps, Sides).
