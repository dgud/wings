%%
%%  wings_color.erl --
%%
%%     Color utilites.
%%
%%  Copyright (c) 2001-2003 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_color.erl,v 1.23 2005/01/23 09:34:56 bjorng Exp $
%%

-module(wings_color).
-export([init/0,choose/1,choose/2,
	 share/1,store/1,average/1,average/2,mix/3,white/0,
	 rgb_to_hsv/3,hsv_to_rgb/3]).

-include("wings.hrl").
-import(lists, [foreach/2,keysearch/3]).

-define(BLACK, {0.0,0.0,0.0}).
-define(WHITE, {1.0,1.0,1.0}).

init() ->
    Black = ?BLACK,
    White = ?WHITE,
    Standard = [Black,White,average([Black,White])],
    put(wings_color_chosen, Black),
    foreach(fun(C0) ->
		    C = wings_util:share(C0),
		    put(C, C)
	    end, Standard).

choose(Done) ->
    Color0 = get(wings_color_chosen),
    choose_1(Color0,
	     fun(Color) ->
		     put(wings_color_chosen, Color),
		     Done(Color)
	     end).

choose(Color, Done) ->
    choose_1(Color, Done).

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

rgb_to_hsv(R, G, B) ->
    case internal_rgb_to_hsv(R, G, B) of
	{undefined,S,V} -> {0.0,S,V};
	HSV -> HSV
    end.

rgb_to_hsv(R, G, B, OldHue) ->
    case internal_rgb_to_hsv(R, G, B) of
	{undefined,S,V} -> {OldHue,S,V};
	HSV -> HSV
    end.

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

convert_hsv(H,V,Min) when H =< 60.0 ->
    Mean = Min + H*(V-Min)/(120.0-H),
    {V, Mean, Min};
convert_hsv(H,V,Min) ->
    Mean = Min+(120-H)*(V-Min)/H,
    {Mean,V,Min}.

%%%
%%% Local functions for color choser.
%%%

-define(COL_PREVIEW_WIDTH, 60).
-define(COL_PREVIEW_HEIGHT, 100).

choose_1(RGB0, Done) ->
    {R1,G1,B1,A1} =
	case RGB0 of
	    {R0,G0,B0}    -> {R0,G0,B0,none};
	    {R0,G0,B0,A0} -> {R0,G0,B0,A0}
	end,
    RGBRange = {0.0,1.0},
    HRange   = {0.0,360.0},
    SIRange  = {0.0,1.0},
    {H1,S1,V1} = rgb_to_hsv(R1, G1, B1),
    PaneColor = wings_pref:get_value(dialog_color),
    Draw = fun(X, Y, W, _, Sto) ->
		   H = ?COL_PREVIEW_HEIGHT,
		   R = gb_trees:get(red, Sto),
		   G = gb_trees:get(green, Sto),
		   B = gb_trees:get(blue, Sto),
		   Half = H div 2,
		   wings_io:blend(PaneColor,
				  fun(Col) ->
					  wings_io:sunken_rect(X, Y, W, H, 
							       {R,G,B}, 
							       Col)
				  end),
		   gl:color3f(R1, G1, B1),
		   gl:rectf(X+0.5, Y+Half, X+W, Y+H),
		   gl:color3b(0, 0, 0),
		   keep
	   end,
    Aslider = case A1 of
		  none -> [];
		  _ ->
		      [separator,
		       {hframe,[{label,"A"},
				{slider,
				 {text,A1,[{key,alpha},{range,RGBRange}]}}]}]
	      end,
    Qs = [{hframe,
	   [{custom,?COL_PREVIEW_WIDTH,?COL_PREVIEW_HEIGHT,Draw},
	    {eyepicker,fun eyepicker_update/2},
	    {vframe,
	     [{hframe,
	       [{vframe,
		 [{hframe,
		   [{label,"R"},
		    {text,R1,color_text_flags(r, {red,green,blue},
					      {hue,sat,val}, RGBRange)}]},
		  {hframe,
		   [{label,"G"},
		    {text,G1,color_text_flags(g, {green,red,blue},
					      {hue,sat,val}, RGBRange)}]},
		  {hframe,
		   [{label,"B"},
		    {text,B1,color_text_flags(b, {blue,red,green},
					      {hue,sat,val}, RGBRange)}]}]},
		{vframe,
		 [{slider,color_slider_flags(r, {red,green,blue},
					     {hue,sat,val}, RGBRange)},
		  {slider,color_slider_flags(g, {green,red,blue},
					     {hue,sat,val}, RGBRange)},
		  {slider,color_slider_flags(b, {blue,red,green},
					     {hue,sat,val}, RGBRange)}]}]},
	      separator,
	      {hframe,
	       [{vframe,
		 [{hframe,
		   [{label,"H"},
		    {text,H1,color_text_flags(h, {hue,sat,val},
					      {red,green,blue}, HRange)}]},
		  {hframe,
		   [{label,"S"},
		    {text,S1,color_text_flags(s, {sat,hue,val},
					      {red,green,blue}, SIRange)}]},
		  {hframe,
		   [{label,"V"},
		    {text,V1,color_text_flags(v, {val,hue,sat},
					      {red,green,blue}, SIRange)}]}]},
		{vframe,
		 [{slider,color_slider_flags(h, {hue,sat,val},
					     {red,green,blue}, HRange)},
		  {slider,color_slider_flags(s, {sat,hue,val},
					     {red,green,blue}, SIRange)},
		  {slider,color_slider_flags(v, {val,hue,sat},
					     {red,green,blue}, SIRange)}]}]}
	      |Aslider]}]}],
    Fun = fun([{red,R},{green,G},{blue,B}|More]) ->
		  RGB = case keysearch(alpha, 1, More) of
			    false -> {R,G,B};
			    {value,{alpha,A}} -> {R,G,B,A}
			end,
		  Done(RGB)
	  end,

wings_ask:dialog(?STR(choose_1,1,"Choose Color"), Qs, Fun).

color_slider_flags(T, {_K1,K2,K3}=K123, Kabc, Range) ->
    [{color,{T,K2,K3}}|color_text_flags(T, K123, Kabc, Range)].

color_text_flags(T, {K1,K2,K3}, Kabc, Range) ->
    [{key,K1},{range,Range},{width,5},{hook,color_update(T, {K2,K3}, Kabc)}].

color_update(T, {K1,K2}, {Ka,Kb,Kc}) ->
    fun (update, {Var,_I,Val,Store0}) ->
	    V1 = gb_trees:get(K1, Store0),
	    V2 = gb_trees:get(K2, Store0),
	    Hue = gb_trees:get(hue, Store0),
	    {Va,Vb,Vc} = color_update(T, Val, V1, V2, Hue),
	    Store1 = gb_trees:update(Var, Val, Store0),
	    Store2 = gb_trees:update(Ka, Va, Store1),
	    Store3 = gb_trees:update(Kb, Vb, Store2),
	    Store = gb_trees:update(Kc, Vc, Store3),
	    {store,Store};
	(_, _) ->
	    void
    end.

eyepicker_update(update, {Var,_I,{R,G,B}=Col,Sto0}) ->
    Sto1 = gb_trees:update(Var, Col, Sto0),
    Sto2 = gb_trees:update(red, R, Sto1),
    Sto3 = gb_trees:update(green, G, Sto2),
    Sto4 = gb_trees:update(blue, B, Sto3),
    {H,S,V} = rgb_to_hsv(R, G, B),
    Sto5 = gb_trees:update(hue, H, Sto4),
    Sto6 = gb_trees:update(sat, S, Sto5),
    Sto = gb_trees:update(val, V, Sto6),
    {store,Sto};
eyepicker_update(_, _) -> void.

color_update(r, R, G, B, OldHue) ->
    rgb_to_hsv(R, G, B, OldHue);
color_update(g, G, R, B, OldHue) ->
    rgb_to_hsv(R, G, B, OldHue);
color_update(b, B, R, G, OldHue) ->
    rgb_to_hsv(R, G, B, OldHue);
color_update(h, H, S, V, _) ->
    hsv_to_rgb(H, S, V);
color_update(s, S, H, V, _) ->
    hsv_to_rgb(H, S, V);
color_update(v, V, H, S, _) ->
    hsv_to_rgb(H, S, V).
