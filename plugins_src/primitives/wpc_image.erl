%%
%%  wpc_image.erl --
%%
%%     Image plane plug-in
%%
%%  Copyright (c) 2002-2004 Bjorn Gustavsson.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wpc_image.erl,v 1.21 2005/12/12 18:00:32 giniu Exp $
%%

-module(wpc_image).
-export([init/0,menu/2,command/2]).

-include_lib("esdl/include/gl.hrl").
-include("e3d.hrl").
-include("e3d_image.hrl").
-include("wings_intl.hrl").

-import(lists, [reverse/1]).

init() ->
    true.

menu({shape}, Menu) ->
    insert_before_more(Menu);
menu(_, Menu) -> Menu.

insert_before_more([H|_]=More) when element(1, element(2, H)) == more ->
    [image_menu(),separator|More];
insert_before_more([H|T]) ->
    [H|insert_before_more(T)];
insert_before_more([]) ->
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
	    wpa:error(?__(1,"Failed to load \"~s\": ~s\n"),
		      [Name,file:format_error(Error)])
    end.

make_image_1(Name0, #e3d_image{type=Type}=Image0) ->
    %% Convert to the format that wings_image wants before padding (faster).
    Image1 = e3d_image:convert(Image0, img_type(Type), 1, lower_left),
    Name = filename:rootname(filename:basename(Name0)),
    #e3d_image{width=W0,height=H0} = Image1,
    Image = pad_image(Image1),
    #e3d_image{width=W,height=H} = Image,
    ImageId = wings_image:new(Name, Image),
    case can_texture_be_loaded(Image) of
	false ->
	    wpa:error(?__(1,"The image cannot be loaded as a texture (it is probably too large)."));
	true ->
	    MaxU = W0/W,
	    MaxV = H0/H,
	    M = [image],
	    Fs = [#e3d_face{vs=[0,3,2,1],tx=[1,0,3,2],mat=M},
		  #e3d_face{vs=[1,2,3,0],tx=[2,3,0,1],mat=M}],
	    UVs = [{0.0,0.0},{MaxU,0.0},{MaxU,MaxV},{0.0,MaxV}],
	    {X,Y} = ratio(W0, H0),
	    Vs = [{0.0,-Y,-X},{0.0,Y,-X},{0.0,Y,X},{0.0,-Y,X}],
	    Mesh = #e3d_mesh{type=polygon,fs=Fs,vs=Vs,tx=UVs},
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
    
can_texture_be_loaded(#e3d_image{width=W,height=H,image=Pixels}) ->
    gl:texImage2D(?GL_PROXY_TEXTURE_2D, 0, ?GL_RGB,
		  W, H, 0, ?GL_RGB, ?GL_UNSIGNED_BYTE, Pixels),
    W == gl:getTexLevelParameteriv(?GL_PROXY_TEXTURE_2D, 0,
				   ?GL_TEXTURE_WIDTH).

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
zeroes(N) when N rem 2 == 0 ->
    Z = zeroes(N div 2),
    [Z|Z];
zeroes(N) ->
    Z = zeroes(N div 2),
    [0,Z|Z].

nearest_power_two(N) ->
    nearest_power_two(N, 1).

nearest_power_two(N, B) when N =< B -> B;
nearest_power_two(N, B) -> nearest_power_two(N, B bsl 1).
