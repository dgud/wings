%%
%%  wp8_jpeg_image.erl --
%%
%%     Plug-in for reading and writing JPEG files
%%     using libjpeg from IJG (Independent JPEG Group).
%%
%%  Copyright (c) 2004 Bjorn Gustavsson
%%
%%  libjpeg is copyright (C) 1991-1998, Thomas G. Lane.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wp8_jpeg_image.erl,v 1.6 2006/01/19 22:01:02 giniu Exp $
%%

-module(wp8_jpeg_image).

-export([init/1,format_error/1]).

-include("e3d_image.hrl").
-include("wings_intl.hrl").

%% Operations supported by driver.
-define(OP_IMAGE_READ, 0).
-define(OP_IMAGE_WRITE, 1).

init(Next) ->
    Dir = filename:dirname(code:which(?MODULE)),
    case erl_ddll:load_driver(Dir, "wings_jpeg_image_drv") of
	ok ->
	    case open_port({spawn,wings_jpeg_image_drv},[]) of
		Port when is_port(Port) ->
		    register(?MODULE, Port),
		    fun(What) ->
			    fileop(What,Next)
		    end;
		_Other ->
		    Next
	    end;
	_ -> Next
    end.

format_error(format) -> ?__(1,"File format not recognized");
format_error({message,Str}) -> binary_to_list(Str);
format_error(_) -> ?__(2,"Unknown error").

fileop({image,formats,Fs0}, Next) ->
    Fs = image_formats(Fs0),
    Next({image,formats,Fs});
fileop({image,read,Prop}=What, Next) ->
    Name = proplists:get_value(filename, Prop),
    case is_format_supported(Name) of
	true -> read_image(Name, Prop);
	false -> Next(What)
    end;
fileop({image,write,Prop}=What, Next) ->
    Name = proplists:get_value(filename, Prop),
    Image = proplists:get_value(image, Prop),
    Ext = lower(filename:extension(Name)),
    case is_format_supported_ext(Ext) of
	true -> write_image(Name, Image);
	false -> Next(What)
    end;
fileop(What, Next) ->
    Next(What).

read_image(Name, Prop) ->
    case file:read_file(Name) of
	{ok,Bin} ->
	    read_image_1(Bin, Prop);
	{error,_}=Error -> Error
    end.
	    
read_image_1(Bin, Prop) ->
    case erlang:port_control(?MODULE, ?OP_IMAGE_READ, Bin) of
	[] -> {error,{none,?MODULE,format}};
	Res -> read_image_2(Res, Prop)
    end.

read_image_2(<<0:32/native,Bin/binary>>, _Prop) ->
    {error,{none,?MODULE,{message,Bin}}};
read_image_2(<<W:32/native,H:32/native,SamplesPerPixel:32/native,
	      Bits/binary>>, Prop) ->
    Type = case SamplesPerPixel of
	       1 -> g8;
	       3 -> r8g8b8
	   end,
    Image = #e3d_image{type=Type,bytes_pp=SamplesPerPixel,
		       alignment=1,order=upper_left,
		       width=W,height=H,image=Bits},
    NeededType = proplists:get_value(type, Prop, Type),
    NeededAlignment = proplists:get_value(alignment, Prop, 1),
    NeededOrder = proplists:get_value(order, Prop, upper_left),
    e3d_image:convert(Image, NeededType, NeededAlignment, NeededOrder).

write_image(Name, #e3d_image{bytes_pp=Bpp,type=Type}=Image0) ->
    {BitsPP,Image} =
	case {Bpp,Type} of
	    {1,g8} ->
		{1,e3d_image:convert(Image0, g8, 1, upper_left)};
	    {3,_} ->
		{3,e3d_image:convert(Image0, r8g8b8, 1, upper_left)};
	    {4,_} ->
		{3,e3d_image:convert(Image0, r8g8b8, 1, upper_left)};
	    _ ->
		{error,{none,?MODULE,format}}
	end,
    Data = [<<(Image#e3d_image.width):32/native,
	     (Image#e3d_image.height):32/native,
	     BitsPP:32/native>>|Image#e3d_image.image],
    case erlang:port_control(?MODULE, ?OP_IMAGE_WRITE, Data) of
	[] -> {error,{none,?MODULE,format}};
	Bin -> file:write_file(Name, Bin)
    end.

is_format_supported(Name) ->
    Ext = lower(filename:extension(Name)),
    is_format_supported_ext(Ext).

is_format_supported_ext(Ext) ->
    lists:keymember(Ext, 1, image_formats([])).

lower([Upper|T]) when $A =< Upper, Upper =< $Z ->
    [Upper-$A+$a|lower(T)];
lower([H|T]) ->
    [H|lower(T)];
lower([]) -> [].

image_formats(Fs) ->
    [{".jpg",?__(1,"JPEG File")}|Fs].
