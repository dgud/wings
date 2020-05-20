%%
%%  wings_color.erl --
%%
%%     Color utilites.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_color).
-export([init/0,choose/1,choose/2,choose/3,
	 share/1,store/1,average/1,average/2,mix/3,white/0,
	 rgb_to_hsv/1,rgb_to_hsv/3,hsv_to_rgb/1,hsv_to_rgb/3,
	 rgba_to_rgb/1, rgb3bv/1, rgb4bv/1, rgb3fv/1, rgb4fv/1,
	 def_palette/0
	]).

-include("wings.hrl").
-import(lists, [foreach/2,keyfind/3]).

-define(BLACK, {0.0,0.0,0.0}).
-define(WHITE, {1.0,1.0,1.0}).

init() ->
    Black = ?BLACK,
    White = ?WHITE,
    Standard = [Black,White,average([Black,White])],
    ?SET(wings_color_chosen, Black),
    wings_pref:set_default(color_dialog_native, true),
    wings_pref:set_default(color_ctrl_palette,
			   [begin G = float(G0), {G,G,G} end
			    || G0 <- lists:seq(0, 255, 255 div 15)]),
    foreach(fun(C0) ->
		    C = wings_util:share(C0),
		    put(C, C)
	    end, Standard).

def_palette() ->
    {fun() -> wings_pref:get_value(color_ctrl_palette) end,
     fun(Palette) when is_list(Palette) ->
	     wings_pref:set_value(color_ctrl_palette, Palette)
     end}.

choose(Done0) ->
    Color0 = ?GET(wings_color_chosen),
    Done = fun(Color) ->
		   ?SET(wings_color_chosen, Color),
		   Done0(Color)
	   end,
    choose(Color0, Done).

choose(Color, Done) ->
    UseNative = wings_pref:get_value(color_dialog_native)
	andalso tuple_size(Color) =:= 3,
    choose(Color, Done, UseNative).

choose(Color, Done, UseNative) ->
    NoOverride = tuple_size(Color) =/= 4,
    choose_1(Color, Done, UseNative andalso NoOverride).

share({Same,Same}) -> {Same,Same};
share({_,_}=UV) -> UV;
share({_,_,_}=RGB) -> share_1(RGB);
share({_,_,_,_}=RGBA) -> share_1(RGBA).

share_1(Color) ->
    case get(Color) of
	undefined -> wings_util:share(Color);
	SharedColor -> SharedColor
    end.

store({_,_,_}=RGB) -> store_1(RGB);
store({_,_,_,_}=RGBA) -> store_1(RGBA).

store_1(Color) ->
    case get(Color) of
	undefined ->
	    C = wings_util:share(Color),
	    put(C, C),
	    C;
	SharedColor -> SharedColor
    end.

mix(_W, Same, Same) -> Same;
mix(Wa, {Ua,Va}, {Ub,Vb}) when is_float(Wa) ->
    Wb = 1.0 - Wa,
    share({Wa*Ua+Wb*Ub,Wa*Va+Wb*Vb});
mix(Wa, {Ra,Ga,Ba}, {Rb,Gb,Bb}) when is_float(Wa) ->
    Wb = 1.0 - Wa,
    share({Wa*Ra+Wb*Rb,Wa*Ga+Wb*Gb,Wa*Ba+Wb*Bb});
mix(Wa, {Ra,Ga,Ba}, {_,_,_,Ab}=B) ->
    mix(Wa, {Ra,Ga,Ba,Ab}, B);
mix(Wa, {_,_,_,Aa}=A, {Rb,Gb,Bb}) ->
    mix(Wa, A, {Rb,Gb,Bb,Aa});
mix(Wa, {Ra,Ga,Ba,Aa}, {Rb,Gb,Bb,Ab}) when is_float(Wa) ->
    Wb = 1.0 - Wa,
    share({Wa*Ra+Wb*Rb,Wa*Ga+Wb*Gb,Wa*Ba+Wb*Bb,Wa*Aa+Wb*Ab});
mix(_, _, _) -> none.

white() ->
    get(?WHITE).

average([none|_]) -> none;
average([H|T]=L) ->
    case classify(T, H) of
	same -> H;
	colors -> share(e3d_vec:average(L));
	uvs -> average_uvs(T, H);
	none -> none
    end.

average(Same, Same) -> Same;
average({U0,V0}, {U1,V1}) when is_float(U0), is_float(V0) ->
    share({(U0+U1)/2,(V0+V1)/2});
average({R0,G0,B0}, {R1,G1,B1}) when is_float(R0), is_float(G0), is_float(B0) ->
    share({(R0+R1)/2,(G0+G1)/2,(B0+B1)/2});
average(_, _) -> none.

classify([], _) -> same;
classify([none|_], _) -> none;
classify([H|T], H) -> classify(T, H);
classify(List, {_,_}) -> classify_uvs(List);
classify(List, {_,_,_}) -> classify_colors(List).

classify_uvs([{_,_}|T]) -> classify_uvs(T);
classify_uvs([_|_]) -> none;
classify_uvs([]) -> uvs.

classify_colors([{_,_,_}|T]) -> classify_colors(T);
classify_colors([_|_]) -> none;
classify_colors([]) -> colors.

average_uvs(T, {V10,V11}) ->
    average_uvs_1(T, V10, V11, length(T)+1).

average_uvs_1([{V10,V11}|T], A0, A1, L)
  when is_float(V10), is_float(V11), is_float(A0), is_float(A1) ->
    average_uvs_1(T, A0+V10, A1+V11, L);
average_uvs_1([], A0, A1, L0) ->
    L = float(L0),
    {A0/L,A1/L}.


rgb3bv({R,G,B}) -> {round(R*255),round(G*255),round(B*255)};
rgb3bv({R,G,B,_}) -> {round(R*255),round(G*255),round(B*255)}.

rgb4bv({R,G,B}) -> {round(R*255),round(G*255), round(B*255), 255};
rgb4bv({R,G,B,A}) -> {round(R*255),round(G*255),round(B*255), round(A*255)}.

rgb3fv({R,G,B}) -> {R/255,G/255,B/255};
rgb3fv({R,G,B,_}) -> {R/255,G/255,B/255}.

rgb4fv({R,G,B}) -> {R/255,G/255,B/255, 1.0};
rgb4fv({R,G,B,A}) -> {R/255,G/255,B/255,A/255}.

rgb_to_hsv({R,G,B}) ->
    rgb_to_hsv(R,G,B).
rgb_to_hsv(R, G, B) ->
    case internal_rgb_to_hsv(R, G, B) of
	{undefined,S,V} -> {0.0,S,V};
	HSV -> HSV
    end.

%% rgb_to_hsv(R, G, B, OldHue) ->
%%     case internal_rgb_to_hsv(R, G, B) of
%% 	{undefined,S,V} -> {OldHue,S,V};
%% 	HSV -> HSV
%%     end.

internal_rgb_to_hsv(R, G, B) ->
    Max = lists:max([R,G,B]),
    Min = lists:min([R,G,B]),
    V = Max,
    {Hue,Sat} = try
		    {if
			 Min == B -> (G-Min)/(R+G-2.0*Min);
			 Min == R -> (1.0+(B-Min)/(B+G-2.0*Min));
			 Min == G -> (2.0+(R-Min)/(B+R-2.0*Min))
		     end*120,(Max-Min)/Max}
		catch
		    error:badarith ->
			{undefined,0.0}
		end,
    {Hue,Sat,V}.

hsv_to_rgb({H,S,V}) ->
    hsv_to_rgb(H,S,V).
hsv_to_rgb(H, S, V) ->
    Min = V*(1-S),
    if
	V < 1.0E-5 ->
	    {0.0,0.0,0.0};
	H =< 120.0 ->
	    convert_hsv(H,V,Min);
	H =< 240.0 ->
	    {G,B,R} = convert_hsv(H-120.0,V,Min),
	    {R,G,B};
	true ->
	    {B,R,G} = convert_hsv(H-240.0,V,Min),
	    {R,G,B}
    end.

rgba_to_rgb({R,G,B,_}) -> {R,G,B};
rgba_to_rgb({_,_,_}=RGB) -> RGB.

convert_hsv(H,V,Min) when H =< 60.0 ->
    Mean = Min + H*(V-Min)/(120.0-H),
    {V, Mean, Min};
convert_hsv(H,V,Min) ->
    Mean = Min+(120-H)*(V-Min)/H,
    {Mean,V,Min}.

%%%
%%% Local functions for color choser.
%%%

choose_1(RGB0, Done, true) ->
    CData = wxColourData:new(),
    wxColourData:setColour(CData, rgb3bv(RGB0)),
    wxColourData:setChooseFull(CData, true),
    {GetPalette,SetPalette} = def_palette(),
    set_palette(GetPalette, CData),
    Dlg = wxColourDialog:new(wings_dialog:get_dialog_parent(), [{data, CData}]),
    wxColourData:destroy(CData),
    case wxDialog:showModal(Dlg) of
	?wxID_CANCEL ->
	    wxColourDialog:destroy(Dlg),
	    keep;
	?wxID_OK ->
	    NewData = wxColourDialog:getColourData(Dlg),
	    SetPalette(get_palette(NewData)),
	    RGBCol = wxColourData:getColour(NewData),
	    wxColourDialog:destroy(Dlg),
	    wings_dialog:return_result(Done, rgb3fv(RGBCol), wings_wm:this())
    end;

choose_1(RGB0, Done, false) ->
    {R1,G1,B1,A1} =
	case RGB0 of
	    {R0,G0,B0}    -> {R0,G0,B0,none};
	    {R0,G0,B0,A0} -> {R0,G0,B0,A0}
	end,
    RGBRange = {0.0,1.0},
    HRange   = {0.0,360.0},
    SIRange  = {0.0,1.0},
    {H1,S1,V1} = rgb_to_hsv(R1, G1, B1),
    Aslider = case A1 of
		  none -> [];
		  _ ->
		      [separator,
		       {hframe,[{label,"A"},
				{slider,
				 {text,A1,[{key,alpha},{range,RGBRange}]}}]}]
	      end,
    CC = [red,green,blue, hue,sat,val],
    Hook = fun({Type, Key}, Value, Fields) ->
		   RGB = case Type of
			     slider -> Value;
			     text ->
				 OLD = wings_dialog:get_value({slider, color}, Fields),
				 update_element(Key, Value, OLD)
			 end,
		   [wings_dialog:set_value({slider, K}, RGB, Fields)
		    || K <- [color|CC], K =/= Key orelse Type =/= slider ],
		   [wings_dialog:set_value({text, K}, get_element(K, RGB), Fields)
		    || K <- CC, K =/= Key orelse Type =/= text],
		   ok
	   end,
    TextOpt = fun(Key, Range) ->
		      [{key,{text,Key}},
		       {range, Range},
		       {hook, Hook},
		       {width, 6}]
	      end,

    SliderOpt = fun(Col) ->
			[{value, {R0,G0,B0}},
			 {color, Col}, {key, {slider, Col}},
			 {hook, Hook}]
		end,
    W = [{width,2}],
    Qs = [{hframe,
	   [{vframe,[panel,
		     {color, {R0,G0,B0}, [{key, {slider, color}},
					  {hook, Hook},
					  {native_dialog, true}]},
		     panel,
		     {color, {R0,G0,B0}, [{static, true}]}]},
	  {vframe,
	   [{hframe,
	     [{label,"R:", W}, {text,R1,TextOpt(red, RGBRange)},
	      {slider, SliderOpt(red)}]},
	    {hframe,
	     [{label,"G:", W}, {text,G1,TextOpt(green, RGBRange)},
	      {slider, SliderOpt(green)}
	     ]},
	    {hframe,
	     [{label,"B:", W}, {text,B1,TextOpt(blue, RGBRange)},
	      {slider, SliderOpt(blue)}
	     ]},
	    separator,
	    {hframe,
	     [{label,"H:", W}, {text,H1,TextOpt(hue, HRange)},
	      {slider, SliderOpt(hue)}]},
	    {hframe,
	     [{label,"S:", W}, {text,S1,TextOpt(sat, SIRange)},
	      {slider, SliderOpt(sat)}
	     ]},
	    {hframe,
	     [{label,"V:", W},{text,V1,TextOpt(val, SIRange)},
	      {slider, SliderOpt(val)}
	     ]}
	    |Aslider], [{proportion, 1}]}]}],

    Fun = fun([{{slider,color},{R,G,B}}|More]) ->
		  RGB = case keyfind(alpha, 1, More) of
			    false -> {R,G,B};
			    {alpha,A} -> {R,G,B,A}
			end,
		  Done(RGB)
	  end,

    wings_dialog:dialog(?STR(choose_1,1,"Choose Color"), Qs, Fun).

element_no(red)   -> {rgb, 1};
element_no(green) -> {rgb, 2};
element_no(blue)  -> {rgb, 3};
element_no(hue)   -> {hsv, 1};
element_no(sat)   -> {hsv, 2};
element_no(val)   -> {hsv, 3}.

update_element(Col, V, RGB) ->
    case element_no(Col) of
	{rgb, Index} -> setelement(Index, RGB, V);
	{hsv, Index} -> hsv_to_rgb(setelement(Index, rgb_to_hsv(RGB), V))
    end.

get_element(Col, RGB) ->
    case element_no(Col) of
	{rgb, Index} -> element(Index, RGB);
	{hsv, Index} -> element(Index, rgb_to_hsv(RGB))
    end.

set_palette(Get, Data) when is_function(Get) ->
    set_palette(Get(), Data, 0).

set_palette([Col|Pal], Data, I) when I < 16 ->
    wxColourData:setCustomColour(Data, I, wings_color:rgb3bv(Col)),
    set_palette(Pal, Data, I+1);
set_palette(_, _, _) -> ok.

get_palette(Data) ->
    get_palette(Data, 0).

get_palette(Data, I) when I < 16 ->
    [rgb3fv(wxColourData:getCustomColour(Data,I))|get_palette(Data, I+1)];
get_palette(_, _) -> [].

%% eyepicker_update(update, {Var,_I,{R,G,B}=Col,Sto0}) ->
%%     Sto1 = gb_trees:update(Var, Col, Sto0),
%%     Sto2 = gb_trees:update(red, R, Sto1),
%%     Sto3 = gb_trees:update(green, G, Sto2),
%%     Sto4 = gb_trees:update(blue, B, Sto3),
%%     {H,S,V} = rgb_to_hsv(R, G, B),
%%     Sto5 = gb_trees:update(hue, H, Sto4),
%%     Sto6 = gb_trees:update(sat, S, Sto5),
%%     Sto = gb_trees:update(val, V, Sto6),
%%     {store,Sto};
%% eyepicker_update(_, _) -> void.
