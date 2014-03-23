%%
%%  wp9_dialogs.erl --
%%
%%     Standard plugin for dialogs.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wp9_dialogs).
-export([init/1]).
-import(lists, [sort/1]).

-include_lib("wings/intl_tools/wings_intl.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-include_lib("wx/include/wx.hrl").  %% includes wx headers

init(Next) ->
    wpa:pref_set_default(?MODULE, utf8, true),
    fun(What) -> ui(What, Next) end.

ui({file,open_dialog,Prop,Cont}, _Next) ->
    Title = proplists:get_value(title, Prop, ?__(1,"Open")),
    file_dialog(?wxFD_OPEN, Prop, Title, Cont);
ui({file,save_dialog,Prop,Cont}, _Next) ->
    Title = proplists:get_value(title, Prop, ?__(2,"Save")),
    file_dialog(?wxFD_SAVE, Prop, Title, Cont);
ui({image,formats,Formats}, _Next) ->
    image_formats(Formats);
ui({image,read,Prop}, _Next) ->
    read_image(Prop);
ui({image,write,Prop}, _Next) ->
    write_image(Prop);
ui(What, Next) -> Next(What).

file_dialog(Type, Prop, Title, Cont) ->
    Frame = get(top_frame),
    DefDir = proplists:get_value(directory, Prop),
    DefName = proplists:get_value(default_filename, Prop, ""),
    Filters = wings_file:file_filters(Prop),
    Dlg = wxFileDialog:new(Frame,
			   [{message, Title},
			    {defaultDir, DefDir},
			    {defaultFile, DefName},
			    {wildCard, Filters},
			    {style, Type}]),
    case wxFileDialog:showModal(Dlg) of
	?wxID_OK ->
	    Dir = wxFileDialog:getDirectory(Dlg),
	    File = wxFileDialog:getFilename(Dlg),
	    wxDialog:destroy(Dlg),
	    Cont(filename:join(Dir, File));
	_Cancel ->
	    wxDialog:destroy(Dlg),
	    keep
    end.

read_image(Prop) ->
    Name = proplists:get_value(filename, Prop),
    case wxImage:loadFile(Image=wxImage:new(), Name) of
	true ->
	    E3d0 = #e3d_image{image=wxImage:getData(Image),
			      width=wxImage:getWidth(Image),
			      height=wxImage:getHeight(Image),
			      order = upper_left
			     },
	    E3d = case wxImage:hasAlpha(Image) of
		      true -> e3d_image:add_alpha(E3d0, wxImage:getAlpha(Image));
		      false -> E3d0
		  end,
	    e3d_image:fix_outtype(Name, E3d, Prop);
	false ->
	    {error, ignore}
    end.

write_image(Prop) ->
    Name  = proplists:get_value(filename, Prop),
    Image0 = proplists:get_value(image, Prop),
    Wx = case Image0 of
	     #e3d_image{bytes_pp=4, width=W, height=H} ->
		 #e3d_image{image=RGB} = e3d_image:convert(Image0, r8g8b8, 1, upper_left),
		 #e3d_image{image=Alpha} = e3d_image:convert(Image0, a8, 1, upper_left),
		 wxImage:new(W,H,RGB,Alpha);
	     #e3d_image{bytes_pp=3, width=W, height=H} ->
		 #e3d_image{image=RGB} = e3d_image:convert(Image0, r8g8b8, 1, upper_left),
		 wxImage:new(W,H,RGB);
	     #e3d_image{bytes_pp=1, width=W, height=H} ->
		 #e3d_image{image=RGB} = e3d_image:convert(Image0, r8g8b8, 1, upper_left),
		 wxImage:new(W,H,RGB)
	 end,
    case wxImage:saveFile(Wx, Name) of
	true -> ok;
	false -> {error, ignore}
    end.

image_formats(Fs0) ->
    Fs1 = [{".bmp",?__(1,"BMP Bitmap File")},
	   {".gif",?__(6,"Compuserve GIF")},  %% only support 8pp
	   {".jpg",?__(5, "JPEG File")},
	   {".png",?__(3,"PNG File")},
	   {".tif",?__(2,"Tiff Bitmap")},
	   {".tga",?__(4,"Targa File")}
	   |Fs0],
    Fs2 = sofs:relation(Fs1),
    Fs3 = sofs:relation_to_family(Fs2),
    Fs = sofs:to_external(Fs3),
    [{Ext,Desc} || {Ext,[Desc|_]} <- Fs].
