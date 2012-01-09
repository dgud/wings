%%
%%  wpc_image.erl --
%%
%%     Image plane plug-in
%%
%%  Copyright (c) 2002-2011 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_image).
-export([init/0,menu/2,command/2]).

-define(NEED_OPENGL,1).
-include("wings.hrl").

-include("e3d.hrl").
-include("e3d_image.hrl").


-import(lists, [reverse/1]).

init() ->
    true.

menu({shape}, Menu) ->
    insert_before_image(Menu);
menu(_, Menu) -> Menu.

insert_before_image([{_,image,_}=Image|Rest]) ->
    [Image,image_menu(),separator|Rest];
insert_before_image([H|T]) ->
    [H|insert_before_image(T)];
insert_before_image([]) ->
    [image_menu()].

image_menu() ->
    {?__(1,"Image Plane..."),image_plane,?__(2,"Create a plane containing an image")}.

command({shape,image_plane}, _St) ->
    make_image();
command({shape,{image_plane,Name}}, _St) ->
    make_image(Name);
command(_, _) -> next.

make_image() ->
    Ps = [{extensions,wpa:image_formats()}],
    wpa:import_filename(Ps, fun(N) -> {shape,{image_plane,N}} end).

make_image(Name) ->
    Props = [{filename,Name}],
    case wpa:image_read(Props) of
	#e3d_image{}=Image ->
	    make_image_1(Name, Image);
	{error,Error} ->
	    wpa:error_msg(?__(1,"Failed to load \"~s\": ~s\n"),
		      [Name,file:format_error(Error)])
    end.

make_image_1(Name0, #e3d_image{type=Type}=Image0) ->
    %% Convert to the format that wings_image wants before padding (faster).
	case wings_image:maybe_exceds_opengl_caps(Image0) of
    {error,GlErr} ->
	    wpa:error_msg(?__(1,"The image cannot be loaded as a texture.~nFile: \"~s\"~n GLU Error: ~p - ~s~n"),
		      [Name0,GlErr, glu:errorString(GlErr)]);
	Image0i -> 
		Image1 = e3d_image:convert(Image0i, img_type(Type), 1, lower_left),
		Name = filename:rootname(filename:basename(Name0)),
		#e3d_image{width=W0,height=H0} = Image1,
		Image = case pad_image(Image1) of
			Image1 ->
				%% The image was not padded, therefore it is safe
				%% to keep the image as external.
				Image0;
			Image2 ->
				%% The image has been padded. We must make sure that
				%% the new image is saved - so force it to be internal.
				Image2#e3d_image{filename=none}
			end,
		#e3d_image{width=W,height=H} = Image,
		ImageId = wings_image:new(Name, Image),
	    MaxU = W0/W,
	    MaxV = H0/H,
	    M = [image],
	    Fs = [#e3d_face{vs=[0,3,2,1]},
		  #e3d_face{vs=[2,3,7,6],tx=[6,2,3,7],mat=M},
		  #e3d_face{vs=[0,4,7,3]},
		  #e3d_face{vs=[1,2,6,5]},
		  #e3d_face{vs=[4,5,6,7]},
		  #e3d_face{vs=[0,1,5,4],tx=[4,0,1,5],mat=M}],
	    UVs = [{0.0,MaxV},{MaxU,MaxV},{0.0,0.0},{MaxU,0.0},
	           {0.0,0.0},{MaxU,0.0},{0.0,MaxV},{MaxU,MaxV}],
	    HardEdges = [{0,3},{2,3},{1,2},{0,1},{3,7},{6,7},
	                 {2,6},{0,4},{4,7},{4,5},{5,6},{1,5}],
	    {X,Y} = ratio(W0, H0),
	    D = 1.0e-3/2,
	    Vs = [{-D,-Y,X},{-D,Y,X},{D,Y,X},{D,-Y,X},
	          {-D,-Y,-X},{-D,Y,-X},{D,Y,-X},{D,-Y,-X}],
	    Mesh = #e3d_mesh{type=polygon,fs=Fs,vs=Vs,tx=UVs,he=HardEdges},
	    Obj = #e3d_object{obj=Mesh},
	    White = wings_color:white(),
	    Black = wings_color:white(),
	    Mat = [{image,
		    [{opengl,[{emission,White},
			      {diffuse,White},
			      {specular,Black},
			      {ambient,Black}]},
		     {maps,[{diffuse,ImageId}]}]}],
	    {new_shape,"image",Obj,Mat}
    end.

img_type(b8g8r8) -> r8g8b8;
img_type(b8g8r8a8) -> r8g8b8a8;
img_type(Type) -> Type.

ratio(D, D) -> {1.0,1.0};
ratio(W, H) when W < H -> {1.0,H/W};
ratio(W, H) -> {W/H,1.0}.
    
pad_image(#e3d_image{width=W0,image=Pixels0,bytes_pp=PP}=Image) ->
    case nearest_power_two(W0) of
	W0 ->
	    pad_image_1(Image);
	W ->
	    Pad = zeroes(PP*(W-W0)),
	    Pixels = pad_rows(Pixels0, PP*W0, Pad, []),
	    pad_image_1(Image#e3d_image{width=W,image=Pixels})
    end.

pad_image_1(#e3d_image{width=W,height=H0,image=Pixels0,bytes_pp=PP}=Image) ->
    case nearest_power_two(H0) of
	H0 ->
	    pad_image_2(Image);
	H ->
	    Pad = zeroes(PP*W*(H-H0)),
	    Pixels = [Pixels0|Pad],
	    pad_image_2(Image#e3d_image{height=H,image=Pixels})
    end.

pad_image_2(#e3d_image{image=Pixels}=Image) when is_list(Pixels) ->
    Image#e3d_image{image=list_to_binary(Pixels)};
pad_image_2(Image) -> Image.

pad_rows(Bin, W, Pad, Acc) ->
    case Bin of
	<<>> -> reverse(Acc);
	<<Row:W/binary,T/binary>> ->
	    pad_rows(T, W, Pad, [[Row|Pad]|Acc])
    end.

zeroes(0) -> [];
zeroes(1) -> [0];
zeroes(N) when N rem 2 =:= 0 ->
    Z = zeroes(N div 2),
    [Z|Z];
zeroes(N) ->
    Z = zeroes(N div 2),
    [0,Z|Z].

nearest_power_two(N) ->
    nearest_power_two(N, 1).

nearest_power_two(N, B) when N =< B -> B;
nearest_power_two(N, B) -> nearest_power_two(N, B bsl 1).
