%%
%%  wp8_mac_image.erl --
%%
%%     Image reading and writing for Mac OS X.
%%
%%  Copyright (c) 2002-2004 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wp8_mac_image.erl,v 1.9 2006/01/20 14:47:03 giniu Exp $
%%

-module(wp8_mac_image).

-export([init/1,format_error/1]).

-include("e3d_image.hrl").
-include("wings_intl.hrl").

%% Operations supported by driver.
-define(OP_IMAGE_READ, 0).
-define(OP_IMAGE_WRITE, 1).

-define(IMG_FORMAT_PNG, 0).
-define(IMG_FORMAT_GIF, 1).
-define(IMG_FORMAT_JPEG, 2).

init(Next) ->
    case os:type() of
	{unix,darwin} ->
	    Dir = filename:dirname(code:which(?MODULE)),
	    case erl_ddll:load_driver(Dir, "mac_wings_image_drv") of
		ok ->
		    case open_port({spawn,"mac_wings_image_drv"},[]) of
			Port when is_port(Port) ->
			    register(?MODULE, Port),
			    fun(What) ->
				    fileop(What,Next)
			    end;
			_ -> Next
		    end;
		_ -> Next
	    end;
	_ -> Next
    end.

format_error(format) -> ?__(1,"Unknown or unsupported format.").

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
	true -> write_image(Name, Ext, Image);
	false -> Next(What)
    end;
fileop(What, Next) ->
    Next(What).

read_image(Name, Prop) ->
    case file:open(Name, [read]) of
	{ok,Fd} ->
	    file:close(Fd),
	    read_image_1(Name, Prop);
	{error,_}=Error -> Error
    end.
	    
read_image_1(Name, Prop) ->
    Data = [Name,0],
    case erlang:port_control(?MODULE, ?OP_IMAGE_READ, Data) of
	[] -> {error,{none,?MODULE,format}};
	Res -> read_image_2(Res, Prop)
    end.

read_image_2(<<W:32/native,H:32/native,SamplesPerPixel0:32/native,BytesPerRow:32,
	      Bits/binary>>, Prop) ->
    SamplesPerPixel = case {BytesPerRow div W,SamplesPerPixel0} of
			  {4,3} -> 4;
			  _ -> SamplesPerPixel0
		      end,
    Type = case SamplesPerPixel of
	       1 -> g8;
	       3 -> r8g8b8;
	       4 -> r8g8b8a8
	   end,
    Al = case BytesPerRow - SamplesPerPixel*W of
	     0 -> 1;
	     1 -> 2;
	     2 -> 4;
	     3 -> 4;
 	     A when A >= 32 -> 64;
 	     A when A >= 16 -> 32;
 	     A when A >= 8 -> 16;
 	     A when A >= 4 -> 8
	 end,
    Image = #e3d_image{type=Type,bytes_pp=SamplesPerPixel,
		       alignment=Al,order=upper_left,
		       width=W,height=H,image=Bits},
    NeededType = proplists:get_value(type, Prop, Type),
    NeededAlignment = proplists:get_value(alignment, Prop, 1),
    NeededOrder = proplists:get_value(order, Prop, upper_left),
    e3d_image:convert(Image, NeededType, NeededAlignment, NeededOrder).

write_image(Name, Ext, Image) ->
    {ok,Tiff} = e3d_image:save_bin(Image, ".tiff"),
    Data = [image_format(Ext)|Tiff],
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
    [{".png",?__(1,"PNG File")},
     {".gif",?__(2,"Compuserve GIF")},
     {".jpg",?__(3,"JPEG File")}|Fs].

image_format(".png") -> ?IMG_FORMAT_PNG;
image_format(".gif") -> ?IMG_FORMAT_GIF;
image_format(".jpg") -> ?IMG_FORMAT_JPEG.
