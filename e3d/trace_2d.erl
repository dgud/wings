
%%
%%  trace_2d.erl --
%%
%%     Automatically determine paths in a bitmap image.
%%
%%  Copyright (c) 2025 Edward Blake
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(trace_2d).
-export([from_image/1,from_image/2,opts/0,opt_default/1,opt_range/1]).

-ifdef(TEST).
-export([t/0,t2/0,t3/0,t4/0,t5/0,t6/0,t7/0,t8/0,t9/0]).
-endif.

-include("e3d.hrl").
-include_lib("e3d_image.hrl").

%%%
%%%

-record(smooth_params, {
    ini=0.6,   %% Initial
    att=0.5,   %% Attenuation with each successive neighboring point
    count=2,   %% How many points on each side to smooth with
    disp=0.5,  %% Point displacement for sharp corners
    dispc=0.5  %% Point displacement correction
}).

%% Use a monochrome e3d_image to create paths
%%
%% Returns {VsArray,[[[Idx|...]=Main|[[Idx|...]|...]=Holes]]}
%%
%% Note that returned VsArray has Y going up (image contour will look
%% upside down) and the scale is the same as whole numbers of pixel coordinates
%% (a 64x64 image would have points ranging from {0.0,0.0,_} to {63.0,63.0,_}).
%% Simply flip Y: [{X,-Y,Z} || {X,Y,Z} <- array:to_list(VsArray)]
%% Faces are already counter-clockwise after flipping Y.
%%
from_image(#e3d_image{}=Img) ->
    from_image(Img, []).
from_image(#e3d_image{}=Img0, Opts) when is_list(Opts) ->
    PointsBudget = proplists:get_value(budget, Opts, opt_default(budget)),
    Img = from_e3d_im(Img0),
    Img_1 = to_shape_1(Img, Opts),
    Img_2 = to_shape_2(Img_1, Opts),
    Img_3 = to_shape_3(Img_2, Opts),
    Img_4 = reset_px_vals_1(Img_3),
    Img_5 = follow_line(fill_pixels(Img_4)),
    Paths = follow_line_1(Img_5),
    Paths_1 = smooth(Paths, smooth_opts(Opts)),
    Paths_2 = less_points_than_budget(Paths_1, PointsBudget),
    Paths_3 = group_paths(Paths_2),
    indexed_paths(Paths_3).



%%
%% The process is:
%% Grayscale denoise -> clamp -> reset -> fill -> follow_lines
%%
%% The grayscale image is denoised and then clamped to a bitmap image
%% with only two intensities, and then the bitmap has pixels filled
%% further based on neighboring pixels so that there are no pixels that
%% has a 0 value on more than one corner of itself, which is very important,
%% as the bitmap will store the path direction in its pixels, so the thinnest
%% area of a shape has to have room for a direction pixel going one direction,
%% and another pixel for the opposite direction to complete a path. After doing
%% some pattern-based fills, A binary denoise is then done to remove any
%% area too small for a path.
%% Then pixel values 0 and 1 are replaced with direction values to make a
%% pixel-based, staircase path. The pixel-based paths are then smoothed
%% and the number of points in the paths reduced until it is below the
%% points budget.
%%

px_at({W, H, Bin}, X, Y)
  when X >= 0, Y >= 0, X < W, Y < H ->
    binary:at(Bin,Y*W+X);
px_at({W, H, _}, X, Y)
  when X < 0; Y < 0; X >= W; Y >= H ->
    0.

%% #e3d_image{} to {W,H,Bin}
%%
from_e3d_im(#e3d_image{type=g8,alignment=1,order=upper_left,bytes_pp=1}=Img) ->
    from_e3d_im_1(Img);
from_e3d_im(#e3d_image{}=Img) ->
    %% e3d_image:convert() can be used to turn 
    %% r8g8b8, r8g8b8a8, etc. to g8
    from_e3d_im(e3d_image:convert(Img, g8, 1, upper_left)).

from_e3d_im_1(#e3d_image{type=g8,width=W,height=H,image=Bin,
                      bytes_pp=BPP,alignment=AB,order=Order}) ->
    if
        BPP =:= 1, AB =:= 1, Order =:= upper_left ->
            {W,H,Bin}
    end.


%% Available opts
opts() ->
    [
        budget, invert, denoise,
        clamp, smooth_ini, smooth_att,
        smooth_count, smooth_disp, smooth_dispc
    ].

%% Default values for opts
opt_default(Param) ->
    case Param of
        budget -> 1000; %% Points Budget
        invert -> false; %% Invert Image before processing
        denoise -> true; %% Denoise image before processing
        clamp -> 127; %% Clamp value from grayscale to monochrome
        smooth_ini -> 0.6; %% Smooth Initial Factor
        smooth_att -> 0.5; %% Smooth Attenuation
        smooth_count -> 2; %% Smooth Point Count
        smooth_disp -> 0.5; %% Smooth Point Displacement
        smooth_dispc -> 0.5 %% Smooth Point Disp. Correction
    end.

opt_range(Param) ->
    case Param of
        clamp -> {1, 254};
        smooth_ini -> {0.2, 0.8};
        smooth_att -> {0.0, 0.9};
        smooth_count -> {0, 4};
        smooth_disp -> {0.45, 1.0};
        smooth_dispc -> {0.0, 0.8};
        _ -> true
    end.

%%

to_shape_1(Img, Opts) ->
    case proplists:get_value(invert, Opts, opt_default(invert)) of
        true ->
            invert_px(Img);
        false ->
            Img
    end.

to_shape_2(Img, Opts) ->
    case proplists:get_value(denoise, Opts, opt_default(denoise)) of
        true ->
            denoise(Img);
        false ->
            Img
    end.

to_shape_3(Img, Opts) ->
    AtVal = proplists:get_value(clamp, Opts, opt_default(clamp)),
    clamp_px_vals(Img, in_range_int(AtVal, opt_range(clamp), opt_default(clamp))).

smooth_opts(Opts) ->
    Ini = proplists:get_value(smooth_ini, Opts, opt_default(smooth_ini)),
    Att = proplists:get_value(smooth_att, Opts, opt_default(smooth_att)),
    Count = proplists:get_value(smooth_count, Opts, opt_default(smooth_count)),
    Disp = proplists:get_value(smooth_disp, Opts, opt_default(smooth_disp)),
    DispC = proplists:get_value(smooth_dispc, Opts, opt_default(smooth_dispc)),
    #smooth_params{
        %% Initial
        ini=in_range(Ini, opt_range(smooth_ini), opt_default(smooth_ini)),
        %% Attenuation with each successive neighboring point
        att=in_range(Att, opt_range(smooth_att), opt_default(smooth_att)),
        %% How many points on each side to smooth with
        count=in_range_int(Count, opt_range(smooth_count), opt_default(smooth_count)),
        %% Point displacement for sharp corners
        disp=in_range(Disp, opt_range(smooth_disp), opt_default(smooth_disp)),
        %% Point displacement correction
        dispc=in_range(DispC, opt_range(smooth_dispc), opt_default(smooth_dispc))
    }.



in_range(Val, {_Min, _Max}, Default)
  when not is_number(Val) ->
    Default;
in_range(Val, {_Min, Max}, _Default)
  when Val > Max ->
    Max;
in_range(Val, {Min, _Max}, _Default)
  when Val < Min ->
    Min;
in_range(Val, {_Min, _Max}, _Default) ->
    float(Val).


in_range_int(Val, {_Min, _Max}, Default)
  when not is_number(Val) ->
    Default;
in_range_int(Val, {_Min, Max}, _Default)
  when Val > Max ->
    Max;
in_range_int(Val, {Min, _Max}, _Default)
  when Val < Min ->
    Min;
in_range_int(Val, {_Min, _Max}, _Default) ->
    round(Val).



%%
%%

%% Invert
%%
%% Invert the grayscale intensity of the image
%%
invert_px({W,H,_}=Img) ->
    case invert_px(Img, 0, 0, [], []) of
        NewList ->
            {W,H,iolist_to_binary(NewList)}
    end.
invert_px({_, H, _}=_, _X, Y, [], OL)
  when Y >= H ->
    lists:reverse(OL);
invert_px({W, H, _Bin}=Img, X, Y, O1, OL)
  when X < W, Y < H ->
    invert_px(Img, X+1, Y, [255-px_at(Img, X, Y)|O1], OL);
invert_px({W, _, _}=Img, X, Y, O1, OL)
  when X >= W ->
    invert_px(Img, 0, Y+1, [], [lists:reverse(O1)|OL]).


%%
%%

%% Grayscale denoise filters
%%
denoise(Img) ->
    dilation(erosion(Img)).

erosion({W,H,_}=Img) ->
    case erosion(Img, 0, 0, [], []) of
        NewList ->
            {W,H,iolist_to_binary(NewList)}
    end.
erosion({_, H, _}=_, _X, Y, [], OL)
  when Y >= H ->
    lists:reverse(OL);
erosion({W, H, _Bin}=Img, X, Y, O1, OL)
  when X < W, Y < H ->
    L = px_at_list_5x5(Img,X,Y),
    Min = lists:foldl(fun erlang:min/2, 255, L),
    erosion(Img, X+1, Y, [Min|O1], OL);
erosion({W, _, _}=Img, X, Y, O1, OL)
  when X >= W ->
    erosion(Img, 0, Y+1, [], [lists:reverse(O1)|OL]).

dilation({W,H,_}=Img) ->
    case dilation(Img, 0, 0, [], []) of
        NewList ->
            {W,H,iolist_to_binary(NewList)}
    end.
dilation({_, H, _}=_, _X, Y, [], OL)
  when Y >= H ->
    lists:reverse(OL);
dilation({W, H, _Bin}=Img, X, Y, O1, OL)
  when X < W, Y < H ->
    L = px_at_list_5x5(Img,X,Y),
    Max = lists:foldl(fun erlang:max/2, 0, L),
    dilation(Img, X+1, Y, [Max|O1], OL);
dilation({W, _, _}=Img, X, Y, O1, OL)
  when X >= W ->
    dilation(Img, 0, Y+1, [], [lists:reverse(O1)|OL]).

px_at_list_2({W,_H,_}=Img,X,Step,I,Y,Size)
  when X >= 0, X < W, I < Size ->
    [ px_at(Img, X, Y)
      | px_at_list_2(Img,X+Step,Step,I+1,Y,Size)];
px_at_list_2(_,_,_,_,_,_) ->
    [].

px_at_list_1({_,H,_}=Img,X,Y,Size)
  when Y >= 0, Y < H ->
    A00 = px_at_list_2(Img, X-1, -1, 0, Y, Size),
    A20 = px_at(Img, X, Y),
    A40 = px_at_list_2(Img, X+1, 1, 0, Y, Size),
    A00 ++ [A20] ++ A40;
px_at_list_1(_,_,_,_) ->
    [].

px_at_list_5x5(Img,X,Y) ->
    px_at_list_1(Img, X, Y-2, 2) ++
    px_at_list_1(Img, X, Y-1, 2) ++
    px_at_list_1(Img, X, Y, 2) ++
    px_at_list_1(Img, X, Y+1, 2) ++
    px_at_list_1(Img, X, Y+2, 2).

px_at_list_3x3(Img,X,Y) ->
    px_at_list_1(Img, X, Y-1, 1) ++
    px_at_list_1(Img, X, Y, 1) ++
    px_at_list_1(Img, X, Y+1, 1).


%% Clamp pixel values 
%%
%% Clamp values where any value below the threshold value is 0,
%% and any equal and above is 255.
%%
clamp_px_vals(Img, AtVal)
  when AtVal < 0 ->
    clamp_px_vals(Img, 0);
clamp_px_vals(Img, AtVal)
  when AtVal > 255 ->
    clamp_px_vals(Img, 255);
clamp_px_vals({W,H,_}=Img, AtVal) ->
    case clamp_px_vals(Img, 0, 0, [], [], round(AtVal)) of
        NewList ->
            {W,H,iolist_to_binary(NewList)}
    end.
clamp_px_vals({_, H, _}=_, _X, Y, [], OL, _AtVal)
  when Y >= H ->
    lists:reverse(OL);
clamp_px_vals({W, H, _Bin}=Img, X, Y, O1, OL, AtVal)
  when X < W, Y < H ->
    Val = px_at(Img, X, Y),
    NewVal = if
        Val >= AtVal  ->
            255;
        true ->
            0
    end,
    clamp_px_vals(Img, X+1, Y, [NewVal|O1], OL, AtVal);
clamp_px_vals({W, _, _}=Img, X, Y, O1, OL, AtVal)
  when X >= W ->
    clamp_px_vals(Img, 0, Y+1, [], [lists:reverse(O1)|OL], AtVal).



%%
%% Change 0...255 pixel values to monochrome 0 and 1 values
%% because other values 2...255 will be used for other data
%% afterwards.
%%
reset_px_vals_1({W,H,_}=Img) ->
    case reset_px_vals_1(Img, 0, 0, [], [], false) of
        {_, NewList} ->
            {W,H,iolist_to_binary(NewList)}
    end.
reset_px_vals_1({_, H, _}=_, _X, Y, [], OL, Changed)
  when Y >= H ->
    {Changed, lists:reverse(OL)};
reset_px_vals_1({W, H, _Bin}=Img, X, Y, O1, OL, Changed)
  when X < W, Y < H ->
    Val = px_at(Img, X, Y),
    NewVal = if
        Val >= 1  ->
            1;
        true ->
            0
    end,
    reset_px_vals_1(Img, X+1, Y, [NewVal|O1], OL, Changed);
reset_px_vals_1({W, _, _}=Img, X, Y, O1, OL, Changed)
  when X >= W ->
    reset_px_vals_1(Img, 0, Y+1, [], [lists:reverse(O1)|OL], Changed).


%% Fill pixels and remove noise
%%
fill_pixels(Img_0) ->
    Img_1 = fill_pixels_1(Img_0),
    Img_2 = fill_pixels_2(Img_1),
    Img_3 = fill_pixels_3(Img_2),
    fill_erosion(fill_dilation(fill_pixels_4(Img_3))).



%% Flip pixel if vertical and horizontal neighbors are different
%%
%% 0 0 0      0 0 0
%% 0 1 0  ->  0 0 0
%% 0 0 0      0 0 0
%%
fill_pixels_1({W,H,_}=Img) ->
    case fill_pixels_1(Img, 0, 0, [], [], false) of
        {true, NewList} ->
            NewBin = iolist_to_binary(NewList),
            fill_pixels_1({W,H,NewBin});
        {false, NewList} ->
            {W,H,iolist_to_binary(NewList)}
    end.
fill_pixels_1({_, H, _}=_, _X, Y, [], OL, Changed)
  when Y >= H ->
    {Changed, lists:reverse(OL)};
fill_pixels_1({W, H, _Bin}=Img, X, Y, O1, OL, Changed)
  when X < W, Y < H ->
    A10 = px_at(Img, X, Y-1),
    A01 = px_at(Img, X-1, Y),
    A11 = px_at(Img, X, Y),
    A21 = px_at(Img, X+1, Y),
    A12 = px_at(Img, X, Y+1),
    if
        A10 =/= A11, A01 =/= A11, A21 =/= A11, A12 =/= A11  ->
            NewVal = majority(A10, A01, A21, A12),
            fill_pixels_1(Img, X+1, Y, [NewVal|O1], OL, true);
        true ->
            fill_pixels_1(Img, X+1, Y, [A11|O1], OL, Changed)
    end;
fill_pixels_1({W, _, _}=Img, X, Y, O1, OL, Changed)
  when X >= W ->
    fill_pixels_1(Img, 0, Y+1, [], [lists:reverse(O1)|OL], Changed).


%% Flip 1 pixel lines if their neighbors are set differently.
%%
%% 1 1 1    1 1 1
%% 1 0 0 -> 1 1 0
%% 1 1 1    1 1 1
%%
fill_pixels_2({W,H,_}=Img) ->
    case fill_pixels_2(Img, 0, 0, [], [], false) of
        {true, NewList} ->
            NewBin = iolist_to_binary(NewList),
            fill_pixels_2({W,H,NewBin});
        {false, NewList} ->
            {W,H,iolist_to_binary(NewList)}
    end.
fill_pixels_2({_, H, _}=_, _X, Y, [], OL, Changed)
  when Y >= H ->
    {Changed,lists:reverse(OL)};
fill_pixels_2({W, H, _Bin}=Img, X, Y, O1, OL, Changed)
  when X < W, Y < H ->
    A10 = px_at(Img, X, Y-1),
    A01 = case O1 of
        [] -> px_at(Img, X-1, Y);
        [Val01|_] -> Val01
    end,
    A11 = px_at(Img, X, Y),
    A21 = px_at(Img, X+1, Y),
    A12 = px_at(Img, X, Y+1),
    if
        A10 =/= A11, A01 =/= A11, (A21 =/= A11 orelse A12 =/= A11)  ->
            NewVal = majority(A10, A01, A21, A12),
            fill_pixels_2(Img, X+1, Y, [NewVal|O1], OL, true);
        true ->
            fill_pixels_2(Img, X+1, Y, [A11|O1], OL, Changed)
    end;
fill_pixels_2({W, _, _}=Img, X, Y, O1, OL, Changed)
  when X >= W ->
    fill_pixels_2(Img, 0, Y+1, [], [lists:reverse(O1)|OL], Changed).


%% Remove 1 pixel gaps
%%
%% 0      0
%% 1  ->  0
%% 0      0
%%
fill_pixels_3({W,H,_}=Img) ->
    case fill_pixels_3(Img, 0, 0, [], [], false) of
        {true, NewList} ->
            NewBin = iolist_to_binary(NewList),
            fill_pixels_3({W,H,NewBin});
        {false, NewList} ->
            {W,H,iolist_to_binary(NewList)}
    end.
fill_pixels_3({_, H, _}=_, _X, Y, [], OL, Changed)
  when Y >= H ->
    {Changed,lists:reverse(OL)};
fill_pixels_3({W, H, _Bin}=Img, X, Y, O1, OL, Changed)
  when X < W, Y < H ->
    A10 = px_at(Img, X, Y-1),
    A01 = px_at(Img, X-1, Y),
    A11 = px_at(Img, X, Y),
    A21 = px_at(Img, X+1, Y),
    A12 = px_at(Img, X, Y+1),
    if
        A10 =:= A12, A10 =/= A11 ->
            fill_pixels_3(Img, X+1, Y, [A10|O1], OL, true);
        A01 =:= A21, A01 =/= A11 ->
            fill_pixels_3(Img, X+1, Y, [A01|O1], OL, true);
        true ->
            fill_pixels_3(Img, X+1, Y, [A11|O1], OL, Changed)
    end;
fill_pixels_3({W, _, _}=Img, X, Y, O1, OL, Changed)
  when X >= W ->
    fill_pixels_3(Img, 0, Y+1, [], [lists:reverse(O1)|OL], Changed).


%% Fill more of single pixel diagonal gaps
%% 
%% 1 1 0      1 1 1
%% 1 1 1  ->  1 1 1
%% 0 1 1      1 1 1
%%
fill_pixels_4({W,H,_}=Img) ->
    case fill_pixels_4(Img, 0, 0, [], [], false) of
        {true, NewList} ->
            NewBin = iolist_to_binary(NewList),
            fill_pixels_4({W,H,NewBin});
        {false, NewList} ->
            {W,H,iolist_to_binary(NewList)}
    end.
fill_pixels_4({_, H, _}=_, _X, Y, [], OL, Changed)
  when Y >= H ->
    {Changed,lists:reverse(OL)};
fill_pixels_4({W, H, _Bin}=Img, X, Y, O1, OL, Changed)
  when X < W, Y < H ->
    A00 = px_at(Img, X-2, Y-2),
    A10 = px_at(Img, X-1, Y-2),
    A20 = px_at(Img, X,   Y-2),
    A30 = px_at(Img, X+1, Y-2),
    A40 = px_at(Img, X+2, Y-2),

    A01 = px_at(Img, X-2, Y-1),
    A11 = px_at(Img, X-1, Y-1),
    A21 = px_at(Img, X,   Y-1),
    A31 = px_at(Img, X+1, Y-1),
    A41 = px_at(Img, X+2, Y-1),
    
    A02 = px_at(Img, X-2, Y),
    A12 = px_at(Img, X-1, Y),
    A22 = px_at(Img, X,   Y), %% In the center which gets changed if it is 0
    A32 = px_at(Img, X+1, Y),
    A42 = px_at(Img, X+2, Y),

    A03 = px_at(Img, X-2, Y+1),
    A13 = px_at(Img, X-1, Y+1),
    A23 = px_at(Img, X,   Y+1),
    A33 = px_at(Img, X+1, Y+1),
    A43 = px_at(Img, X+2, Y+1),

    A04 = px_at(Img, X-2, Y+2),
    A14 = px_at(Img, X-1, Y+2),
    A24 = px_at(Img, X,   Y+2),
    A34 = px_at(Img, X+1, Y+2),
    A44 = px_at(Img, X+2, Y+2),
    
    if
        A22 =:= 0 ->
            if
                %% 'H' is for 'here' (A22)
            
                %% A22=H A32=1 A42=1
                %% A23=1 A33=1 A43=1
                %% A24=1 A34=1 A44=0
                A32 =:= 1, A42 =:= 1,
                A23 =:= 1, A33 =:= 1, A43 =:= 1,
                A24 =:= 1, A34 =:= 1, A44 =:= 0 ->
                    fill_pixels_4(Img, X+1, Y, [1|O1], OL, true);

                %% A00=0 A10=1 A20=1
                %% A01=1 A11=1 A21=1
                %% A02=1 A12=1 A22=H
                A00 =:= 0, A10 =:= 1, A20 =:= 1,
                A01 =:= 1, A11 =:= 1, A21 =:= 1,
                A02 =:= 1, A12 =:= 1  ->
                    fill_pixels_4(Img, X+1, Y, [1|O1], OL, true);

                %% A20=1 A30=1 A40=0
                %% A21=1 A31=1 A41=1
                %% A22=H A32=1 A42=1
                A20 =:= 1, A30 =:= 1, A40 =:= 0,
                A21 =:= 1, A31 =:= 1, A41 =:= 1,
                A32 =:= 1, A42 =:= 1  ->
                    fill_pixels_4(Img, X+1, Y, [1|O1], OL, true);

                %% A02=1 A12=1 A22=H
                %% A03=1 A13=1 A23=1
                %% A04=0 A14=1 A24=1
                A02 =:= 1, A12 =:= 1,
                A03 =:= 1, A13 =:= 1, A23 =:= 1,
                A04 =:= 0, A14 =:= 1, A24 =:= 1  ->
                    fill_pixels_4(Img, X+1, Y, [1|O1], OL, true);

                true ->
                    fill_pixels_4(Img, X+1, Y, [A22|O1], OL, Changed)
            end;
        true ->
            fill_pixels_4(Img, X+1, Y, [A22|O1], OL, Changed)
    end;
fill_pixels_4({W, _, _}=Img, X, Y, O1, OL, Changed)
  when X >= W ->
    fill_pixels_4(Img, 0, Y+1, [], [lists:reverse(O1)|OL], Changed).


%% 3x3 binary erosion
%%
fill_erosion({W,H,_}=Img) ->
    case fill_erosion(Img, 0, 0, [], []) of
        NewList ->
            {W,H,iolist_to_binary(NewList)}
    end.
fill_erosion({_, H, _}=_, _X, Y, [], OL)
  when Y >= H ->
    lists:reverse(OL);
fill_erosion({W, H, _Bin}=Img, X, Y, O1, OL)
  when X < W, Y < H ->
    L = px_at_list_3x3(Img,X,Y),
    Min = lists:foldl(fun erlang:min/2, 1, L),
    fill_erosion(Img, X+1, Y, [Min|O1], OL);
fill_erosion({W, _, _}=Img, X, Y, O1, OL)
  when X >= W ->
    fill_erosion(Img, 0, Y+1, [], [lists:reverse(O1)|OL]).

%% 3x3 binary dilation
%%
fill_dilation({W,H,_}=Img) ->
    case fill_dilation(Img, 0, 0, [], []) of
        NewList ->
            {W,H,iolist_to_binary(NewList)}
    end.
fill_dilation({_, H, _}=_, _X, Y, [], OL)
  when Y >= H ->
    lists:reverse(OL);
fill_dilation({W, H, _Bin}=Img, X, Y, O1, OL)
  when X < W, Y < H ->
    L = px_at_list_3x3(Img,X,Y),
    Max = lists:foldl(fun erlang:max/2, 0, L),
    fill_dilation(Img, X+1, Y, [Max|O1], OL);
fill_dilation({W, _, _}=Img, X, Y, O1, OL)
  when X >= W ->
    fill_dilation(Img, 0, Y+1, [], [lists:reverse(O1)|OL]).


majority(A,A,_,_) -> A;
majority(A,_,A,_) -> A;
majority(A,_,_,A) -> A;
majority(_,A,A,_) -> A;
majority(_,A,_,A) -> A;
majority(_,_,A,A) -> A;
majority(A,_,_,_) -> A.


follow_line(Img) ->
    follow_line_0(Img).

-define(N, 10).
-define(E, 11).
-define(W, 13).
-define(S, 12).

%% Set directions of each pixel based on its neighbors
%%
follow_line_0({W,H,_}=Img) ->
    case follow_line_0(Img, 0, 0, [], [], false) of
        {_, NewList} ->
            {W,H,iolist_to_binary(NewList)}
    end.
follow_line_0({_, H, _}=_, _X, Y, [], OL, Changed)
  when Y >= H ->
    {Changed,lists:reverse(OL)};
follow_line_0({W, H, _Bin}=Img, X, Y, O1, OL, Changed)
  when X < W, Y < H ->
    A00 = px_at(Img, X-1, Y-1),
    A10 = px_at(Img, X, Y-1),
    A20 = px_at(Img, X+1, Y-1),
    A01 = px_at(Img, X-1, Y),
    A11 = px_at(Img, X, Y),
    A21 = px_at(Img, X+1, Y),
    A02 = px_at(Img, X-1, Y+1),
    A12 = px_at(Img, X, Y+1),
    A22 = px_at(Img, X+1, Y+1),
    if
        %% Set directions at pixel corners:
        %% 1 = the pixel value is 1 (path can go this way)
        %% 0 = the pixel value is 0
        %% - = the pixel value doesn't matter
        %% The center always matches as 1, but will be set afterwards to:
        %% N (north), E (east), S (south), W (west).

        %% 0  1  -
        %% -  N  -
        %% -  -  -
        A00 =:= 0, A10 =:= 1, A11 =:= 1 ->
            follow_line_0(Img, X+1, Y, [?N|O1], OL, true);

        %% -  -  0
        %% -  E  1
        %% -  -  -
        A20 =:= 0, A11 =:= 1, A21 =:= 1 ->
            follow_line_0(Img, X+1, Y, [?E|O1], OL, true);

        %% -  -  -
        %% -  S  -
        %% -  1  0
        A12 =:= 1, A11 =:= 1, A22 =:= 0 ->
            follow_line_0(Img, X+1, Y, [?S|O1], OL, true);

        %% -  -  -
        %% 1  W  -
        %% 0  -  -
        A01 =:= 1, A11 =:= 1, A02 =:= 0 ->
            follow_line_0(Img, X+1, Y, [?W|O1], OL, true);


        %% -  1  -
        %% 0  N  -
        %% -  -  -
        A01 =:= 0, A10 =:= 1, A11 =:= 1 ->
            follow_line_0(Img, X+1, Y, [?N|O1], OL, true);

        %% -  0  -
        %% -  E  1
        %% -  -  -
        A10 =:= 0, A11 =:= 1, A21 =:= 1 ->
            follow_line_0(Img, X+1, Y, [?E|O1], OL, true);

        %% -  -  -
        %% -  S  0
        %% -  1  -
        A12 =:= 1, A11 =:= 1, A21 =:= 0 ->
            follow_line_0(Img, X+1, Y, [?S|O1], OL, true);

        %% -  -  -
        %% 1  W  -
        %% -  0  -
        A01 =:= 1, A11 =:= 1, A12 =:= 0 ->
            follow_line_0(Img, X+1, Y, [?W|O1], OL, true);

        true ->
            follow_line_0(Img, X+1, Y, [A11|O1], OL, Changed)
    end;
follow_line_0({W, _, _}=Img, X, Y, O1, OL, Changed)
  when X >= W ->
    follow_line_0(Img, 0, Y+1, [], [lists:reverse(O1)|OL], Changed).




%% Follow direction pixels into pixel paths.
%%
follow_line_1(Img) ->
    follow_line_1(Img, 0, 0, [], sets:new()).
follow_line_1({_, H, _}=_, _X, Y, OL, _Already)
  when Y >= H ->
    lists:reverse(OL);
follow_line_1({W, H, _Bin}=Img, X, Y, OL, Already)
  when X < W, Y < H ->
    A11 = px_at(Img, X, Y),
    if
        A11 =:= ?N; A11 =:= ?E; A11 =:= ?S; A11 =:= ?W ->
            case sets:is_element({X,Y}, Already) of
                false ->
                    {Already_1, Path} = follow_line_2(Img, X, Y, Already),
                    follow_line_1(Img, X+1, Y, [Path|OL], Already_1);
                true ->
                    follow_line_1(Img, X+1, Y, OL, Already)
            end;
        true ->
            follow_line_1(Img, X+1, Y, OL, Already)
    end;
follow_line_1({W, _, _}=Img, X, Y, OL, Already)
  when X >= W ->
    follow_line_1(Img, 0, Y+1, OL, Already).


follow_line_2(Img, X, Y, Already) ->
    Already_1 = sets:add_element({X,Y}, Already),
    {X_1,Y_1} = follow_line_d(X,Y,px_at(Img, X, Y)),
    follow_line_2(Img, X_1,Y_1, X, Y, Already_1, [{X,Y}]).

follow_line_2(_, X, Y, X, Y, Already, Path) ->
    {Already, Path};
follow_line_2(Img, X, Y, X0, Y0, Already, Path) ->
    Already_1 = sets:add_element({X,Y}, Already),
    {X_1,Y_1} = follow_line_d(X,Y,px_at(Img, X, Y)),
    follow_line_2(Img, X_1, Y_1, X0, Y0, Already_1, [{X,Y}|Path]).

follow_line_d(X,Y,A) ->
    if
        A =:= ?N ->
            %% Go north
            {X, Y-1};
        A =:= ?E ->
            %% Go east
            {X+1, Y};
        A =:= ?S ->
            %% Go south
            {X, Y+1};
        A =:= ?W ->
            %% Go west
            {X-1, Y}
    end.


%%%
%%%

%%
%% Smooth paths 
%%

smooth(L, SmoothParams) ->
    [smooth_path(A, SmoothParams) || A <- L].

smooth_path(L, SmoothParams) ->
    smooth_path(L, SmoothParams, L, lists:reverse(L), [], [], []).
smooth_path([], SmoothParams, _FullList, _FullListRev, _PrevL, OL, OL2) ->
    adj_corners(lists:zip(OL,OL2),SmoothParams);
smooth_path([B|L], SmoothParams, FullList, FullListRev, PrevL, OL, OL2) ->
    {B_1,C} = smooth_pt(B, SmoothParams, L++FullList, PrevL++FullListRev),
    smooth_path(L, SmoothParams, FullList, FullListRev, [B|PrevL], [B_1|OL], [C|OL2]).

smooth_pt({X0,Y0}, #smooth_params{ini=Initial,att=Imp,count=Count,disp=Dp,dispc=Co}, L, L2) ->
    {X1,Y1} = smooth_pt_1(X0, Y0, L, L2, Initial, Imp, Count),
    if
        %% If the original point and the new smoothed point
        %% deviate too much then we set it as a corner and
        %% readjust back with a point displacement correction.
        %%
        abs(X1-X0) > Dp, abs(Y1-Y0) > Dp ->
            {{X1*Co+X0*(1.0-Co),Y1*Co+Y0*(1.0-Co)}, true};
        true ->
            {{X1,Y1}, false}
    end.

smooth_pt_1(XA, YA, _, _, _, _, 0) ->
    {XA,YA};
smooth_pt_1(XA, YA, L1, L2, _, _, _)
  when L1 =:= []; L2 =:= [] ->
    {XA,YA};
smooth_pt_1(XA, YA, [{X1,Y1}|L], [{X2,Y2}|L2], Amt, Imp, Count)
  when Count > 0 ->
    X = XA*(1.0-Amt) + (X1+X2)*0.5*Amt,
    Y = YA*(1.0-Amt) + (Y1+Y2)*0.5*Amt,
    smooth_pt_1(X, Y, L, L2, Amt*Imp, Imp, Count-1).


%% Readjust points near the corners after smoothing.
%%
adj_corners(L, SmoothParams) ->
    adj_corners(L, SmoothParams, L, lists:reverse(L), [], []).
adj_corners([], _SmoothParams, _FullList, _FullListRev, _PrevL, OL) ->
    [Point || {Point,_} <- OL];
adj_corners([B|L], SmoothParams, FullList, FullListRev, PrevL, OL) ->
    B_1 = adj_corner_pt(B, SmoothParams, L++FullList, PrevL++FullListRev),
    adj_corners(L, SmoothParams, FullList, FullListRev, [B|PrevL], [B_1|OL]).

adj_corner_pt({{_X0,_Y0}=Point,true}, _SmoothParams, _, _) ->
    {Point,true};
adj_corner_pt({{X0,Y0},false}, #smooth_params{ini=Initial,att=Imp,count=Count}, L, L2) ->
    {X1,Y1} = adj_corner_pt_1(X0, Y0, L, L2, Initial, Imp, Count),
    {{X1,Y1}, false}.

adj_corner_pt_1(XA, YA, _, _, _, _, 0) ->
    {XA,YA};
adj_corner_pt_1(XA, YA, L1, L2, _, _, _)
  when L1 =:= []; L2 =:= [] ->
    {XA,YA};
adj_corner_pt_1(XA, YA, [{{X1,Y1},true}|_], [{_,_}|_], Amt, _Imp, Count)
  when Count > 0 ->
    X = XA*(1.0-Amt) + X1*Amt,
    Y = YA*(1.0-Amt) + Y1*Amt,
    {X,Y};
adj_corner_pt_1(XA, YA, [{_,_}|_], [{{X2,Y2},true}|_], Amt, _Imp, Count)
  when Count > 0 ->
    X = XA*(1.0-Amt) + X2*Amt,
    Y = YA*(1.0-Amt) + Y2*Amt,
    {X,Y};
adj_corner_pt_1(XA, YA, [{_,false}|L], [{_,false}|L2], Amt, Imp, Count)
  when Count > 0 ->
    adj_corner_pt_1(XA, YA, L, L2, Amt*Imp, Imp, Count-1).



%%%
%%%

%% Determine the angle differences along path
%%
angle_amounts([First|_]=Path) ->
    [Prev|_]=lists:reverse(Path),
    angle_amounts(Path, Prev, First, []).
angle_amounts([Point1], Prev, First, OL) ->
    lists:reverse([
        abs(
          angle_of_line(Prev,Point1)-
          angle_of_line(Point1,First))
        |OL]);
angle_amounts([Point1|[Point2|_]=L], Prev, First, OL) ->
    angle_amounts(L, Point1, First, [
        abs(
          angle_of_line(Prev,Point1)-
          angle_of_line(Point1,Point2))
        |OL]).

angle_of_line({A1_X,A1_Y}, {A2_X,A2_Y})
  when A1_X =:= A2_X ->
    if
        A2_Y > A1_Y ->
            0.5 * math:pi();
        true ->
            1.5 * math:pi()
    end;
angle_of_line({A1_X,A1_Y}, {A2_X,A2_Y}) ->
    R0 = (A2_Y - A1_Y) / (A2_X - A1_X),
    if
        abs(R0) < ?EPSILON ->
            if
                A2_X < A1_X ->
                    math:pi();
                true ->
                    0
            end;
        true ->
            Ang = math:atan(R0),
            if
                A2_X > A1_X ->
                    Ang;
                true ->
                    Ang + math:pi()
            end
    end.


%% Reduce the number of points in the paths by removing
%% shallow angled points until it meets the points budget.
%%
less_points_than_budget(Paths, MaxPoints) ->
    %% Remove very shallow angles
    Paths_1 = lists:foldl(
        fun(MP,P) -> less_points(P,MP) end,
        Paths,
        [0.00125,0.0025,0.005,0.0125,0.025,0.05]),
    less_points_than_budget(Paths_1, MaxPoints, 0.15).
less_points_than_budget(Paths, MaxPoints, Cutoff) ->
    Paths_1 = less_points(Paths, Cutoff),
    PointCount = lists:foldl(fun erlang:max/2, 0, [length(P)||P<-Paths_1]),
    if
        %% One of the paths turned out with too many points.
        %% Try again with after increasing a bit the angle minimum.
        PointCount > MaxPoints ->
            less_points_than_budget(Paths_1, MaxPoints, Cutoff+0.05);
        true ->
            Paths_1
    end.

less_points(Paths, Cutoff) ->
    [less_points_path(P, angle_amounts(P), Cutoff) || P <- Paths].
less_points_path(Path, AngleAmts, Cutoff) ->
    less_points_path(Path, AngleAmts, Cutoff, []).
less_points_path([], [], _Cutoff, OL) ->
    lists:reverse(OL);
less_points_path([_|Path], [Angle|AngleAmts], Cutoff, OL)
  when Angle < Cutoff ->
    less_points_path(Path, AngleAmts, Cutoff, OL);
less_points_path([Point|Path], [Angle|AngleAmts], Cutoff, OL)
  when Angle >= Cutoff ->
    less_points_path(Path, AngleAmts, Cutoff, [Point|OL]).

%%%
%%%


%% Group together paths and holes
group_paths(Paths) ->
    group_paths(Paths, [],[]).
group_paths([],Paths,Holes) ->
    Paths_1 = lists:usort(
        fun(A,B) ->
            path_rc_a(A) < path_rc_a(B)
        end, Paths),
    group_paths_1(Paths_1, Holes);
group_paths([P|Paths],OL,Holes) ->
    case is_path(P) of
        true ->
            group_paths(Paths, [P|OL],Holes);
        false ->
            group_paths(Paths, OL, [P|Holes]);
        remove ->
            group_paths(Paths, OL, Holes)
    end.

group_paths_1(Paths,Holes) ->
    group_paths_1(Paths,Holes,[]).
group_paths_1([],_Holes,OL) ->
    OL;
group_paths_1([P|Paths],Holes,OL) ->
    {InPath, Holes_1} = lists:partition(fun (B) -> path_inside(B,P) end, Holes),
    group_paths_1(Paths,Holes_1,[[P|InPath]|OL]).

is_path(Path) ->
    [PLast|_] = lists:reverse(Path),
    MinY = lists:foldl(
        fun erlang:min/2, 1000000,
        [Y || {_,Y} <- Path]),
    is_path(Path, MinY, PLast).

is_path([{_X1,Y1},{X3,_}|_], MinY, {X2,_})
  when Y1 =< MinY ->
    (X2-X3) >= 0;
is_path([P1|Path], MinY, _) ->
    is_path(Path, MinY, P1);
is_path([], _MinY, {_X2,_}) ->
    %% Something went wrong
    remove.

path_inside(A,B) ->
    {MinXA,MaxXA,MinYA,MaxYA} = path_rc(A),
    {MinXB,MaxXB,MinYB,MaxYB} = path_rc(B),
    if
        MinXA > MinXB, MinYA > MinYB,
        MaxXA < MaxXB, MaxYA < MaxYB ->
            true;
        true ->
            false
    end.

path_rc_a(Paths) ->
    {MinX,MaxX,MinY,MaxY} = path_rc(Paths),
    (MaxX - MinX) * (MaxY - MinY).

path_rc([{X,Y}|Paths]) ->
    path_rc(Paths, X,X,Y,Y).
path_rc([], MinX,MaxX,MinY,MaxY) ->
    {MinX,MaxX,MinY,MaxY};
path_rc([{X,Y}|Paths], MinX,MaxX,MinY,MaxY) ->
    path_rc(Paths, min(X,MinX),max(X,MaxX),min(Y,MinY),max(Y,MaxY)).
    
%%%
%%%

%% Change to indexed paths
%%
indexed_paths(Paths) ->
    indexed_paths(Paths, array:new(), #{}, []).
indexed_paths([], Arr, _Sets, OL) ->
    {Arr, lists:reverse(OL)};
indexed_paths([P|Paths], Arr, Sets, OL) ->
    {Arr_1, Sets_1, P_1} = indexed_paths_1(P, Arr, Sets),
    indexed_paths(Paths, Arr_1, Sets_1, [P_1|OL]).

indexed_paths_1(Paths, Arr, Sets) ->
    indexed_paths_1(Paths, Arr, Sets, []).
indexed_paths_1([], Arr, Sets, OL) ->
    {Arr, Sets, lists:reverse(OL)};
indexed_paths_1([P|Path], Arr, Sets, OL) ->
    {Arr_1, Sets_1, P_1} = indexed_paths_2(P, Arr, Sets),
    indexed_paths_1(Path, Arr_1, Sets_1, [P_1|OL]).

indexed_paths_2(Paths, Arr, Sets) ->
    indexed_paths_2(Paths, Arr, Sets, []).
indexed_paths_2([], Arr, Sets, OL) ->
    {Arr, Sets, lists:reverse(OL)};
indexed_paths_2([{PX,PY}=P|Path], Arr, Sets, OL) ->
    case maps:find(P, Sets) of
        {ok,Idx} ->
            indexed_paths_2(Path, Arr, Sets, [Idx|OL]);
        error ->
            Idx = array:size(Arr),
            Arr_1 = array:set(Idx, {float(PX),float(PY),0.0}, Arr),
            Sets_1 = maps:put(P, Idx, Sets),
            indexed_paths_2(Path, Arr_1, Sets_1, [Idx|OL])
    end.


%%%
%%%

-ifdef(TEST).

%%
%% Test Code
%%

print_pixels(Img) ->
    print_pixels(Img,0,0).
print_pixels({_,H,_}=_Img,_X,Y) when Y >= H ->
    ok;
print_pixels({W,_,_}=Img,X,Y) when X < W -> 
    case px_at(Img,X,Y) of
        0  -> io:format(" ");
        1  -> io:format("#");
        ?N -> io:format("n");
        ?E -> io:format("e");
        ?S -> io:format("s");
        ?W -> io:format("w")
    end,
    print_pixels(Img,X+1,Y);
print_pixels({W,_,_}=Img,X,Y) when X >= W -> 
    io:format("\n", []),
    print_pixels(Img,0,Y+1).


print_im(Img) ->
    print_im(Img,0,0).
print_im({_,H,_}=_Img,_X,Y) when Y >= H ->
    ok;
print_im({W,_,_}=Img,X,Y) when X < W -> 
    io:format("~c", [round(px_at(Img,X,Y)/4)+32]),
    print_im(Img,X+1,Y);
print_im({W,_,_}=Img,X,Y) when X >= W -> 
    io:format("\n", []),
    print_im(Img,0,Y+1).

t4() ->
    Img = {10, 10, <<
        2,2,42,52,62, 72,62,82,2,2,
        2,20,20,232,232, 72,82,52,122,122,
        2,42,102,20,112, 82,72,72,122,122,
        2,42,102,120,102, 112,52,22,122,122,
        2,2,62,220,112, 72,102,72,122,122,

        2,122,122,122,232, 52,82,202,122,122,
        2,2,22,122,232, 32,72,102,102,122,
        2,2,22,152,132, 42,62,102,222,122,
        2,2,2,102,102, 102,52,132,12,2,
        2,2,2,2,2, 2,2,2,2,2
    >>},
    io:format("-----------~n", []),
    print_im(Img),
    io:format("-----------~n", []),
    print_im(invert_px(Img)).


t3() ->
    Img = {10, 10, <<
        2,2,42,52,62, 72,62,82,2,2,
        2,20,20,232,232, 72,82,52,122,122,
        2,42,102,20,112, 82,72,72,122,122,
        2,42,102,120,102, 112,52,22,122,122,
        2,2,62,220,112, 72,102,72,122,122,

        2,122,122,122,232, 52,82,202,122,122,
        2,2,22,122,232, 32,72,102,102,122,
        2,2,22,152,132, 42,62,102,222,122,
        2,2,2,102,102, 102,52,132,12,2,
        2,2,2,2,2, 2,2,2,2,2
    >>},
    io:format("-----------~n", []),
    print_im(Img),
    io:format("-----------~n", []),
    print_im(denoise(Img)).

t2() ->
    Img=reset_px_vals_1(clamp_px_vals({2,2,<<255,170,120,0>>}, 140)),
    print_pixels(Img).

t() ->
    Img = {20, 20, <<
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,1,1,1,0,0,
        0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
        0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
        0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
        0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
        0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
        0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,0,0,
        0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,1,1,1,0,
        0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,0,0,
        0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,1,1,1,1,0,
        0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,1,1,1,1,0,
        0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,
        0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
        0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
        0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
        0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
        0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    >>},
    Img_2 = follow_line(fill_pixels(Img)),
    print_pixels(Img_2),
    Paths = follow_line_1(Img_2),
    Paths_1 = smooth(Paths, #smooth_params{}),
    Paths_2 = less_points_than_budget(Paths_1, 1000),
    show_points(Paths_2).

t5() ->
    angle_of_line({0,0},{1,0}).

show_points(Paths) ->
    Screen=array:new(64*45),
    {_,Screen1}=lists:foldl(fun(Path, {Idx,Acc}) ->
        Acc2=lists:foldl(fun({X,Y}, Acc1) ->
            X1 = round(X*2+3),
            Y1 = round(Y*2+3),
            array:set(X1+(Y1*64), $A+Idx, Acc1)
        end, Acc, Path),
        {Idx+1,Acc2}
    end, {0,Screen}, Paths),
    show_points_print(array:to_list(Screen1)).

show_points_print([]) ->
    ok;
show_points_print(Screen) ->
    {Row,List} = lists:split(64, Screen),
    io:format("~s~n", [[if C =:= undefined -> 32; true -> C end || C <- Row]]),
    show_points_print(List).

t6() ->
    Path = [
        {1,1}, {2,1}, {3,1},
        {3,2}, {4,2}, {5,2}, {6,2},
        {6,3}, {7,3}, {8,3}, {9,3},
        {9,4}, {9,3}, {9,2}, {9,1}
    ],
    Path_1 = smooth_path(Path, #smooth_params{}),
    Path_2 = less_points_than_budget([Path_1], 100),
    show_points(Path_2),
    Path_2.

t7() ->
    from_e3d_im(#e3d_image{
        type=g8,bytes_pp=1,alignment=1,order=lower_left,
        width=3,height=3,image= <<1,2,3, 4,5,6, 7,8,9>>}).

t8() ->
    Path1 = [{1,28},{8,28},{8,1},{1,1}],
    Path2 = [{6,3},{6,6},{3,6},{3,3}],
    Path3 = [{6,13},{6,26},{3,26},{3,13}],

    Path4 = [{3.5,13.5},{3.5,25.5},{5.5,25.5},{5.5,13.5}],
    Path5 = [{5,15},{5,24},{4,24},{4,15}],
    
    Paths = [Path1,Path2,Path3,Path4,Path5],
    
    P_1 = group_paths(Paths),
    indexed_paths(P_1).

t9() ->
    Img = {20, 20, <<
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,1,1,1,1,1,1,1,1,1,0,0,0,0,0,1,1,1,0,0,
        0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,
        0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
        0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
        0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
        0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
        0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,0,0,
        0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,1,1,1,1,0,
        0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,0,0,
        0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,1,1,1,1,0,
        0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,1,1,1,1,0,
        0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,
        0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
        0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
        0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
        0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
        0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    >>},
    Img_2 = follow_line(fill_pixels(Img)),
    Paths_0 = follow_line_1(Img_2),
    Paths_1 = smooth(Paths_0, #smooth_params{}),
    Paths_2 = less_points_than_budget(Paths_1, 1000),
    Paths_3 = group_paths(Paths_2),
    indexed_paths(Paths_3).



-endif. % TEST

