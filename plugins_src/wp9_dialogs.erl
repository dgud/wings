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
    file_dialog(?wxFD_SAVE bor ?wxFD_OVERWRITE_PROMPT, Prop, Title, Cont);
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
    Multiple = proplists:get_value(multiple, Prop, false),
    Type0 = if Multiple =:= true -> Type bor ?wxFD_MULTIPLE;
               true -> Type
            end,
    Dlg = wxFileDialog:new(Frame,
                           [{message, Title},
                            {defaultDir, DefDir},
                            {defaultFile, DefName},
                            {wildCard, Filters},
                            {style, Type0}]),
    case wxFileDialog:showModal(Dlg) of
        ?wxID_OK ->
            Dir = wxFileDialog:getDirectory(Dlg),
            File = if Multiple =:= true ->
                           wxFileDialog:getFilenames(Dlg);
                      true ->
                           wxFileDialog:getFilename(Dlg)
                   end,
            wxDialog:destroy(Dlg),
            if Multiple =:= true ->
                    [wings_wm:psend(Cont(filename:join(Dir, File0)),wings_wm:get_current_state()) || File0 <- File],
                    keep;
               true ->
                    Cont(filename:join(Dir, File))
            end;
        _Cancel ->
            wxDialog:destroy(Dlg),
            keep
    end.

read_image(Prop) ->
    Name = proplists:get_value(filename, Prop),
    BlockWxMsgs = wxLogNull:new(),
    case wxImage:loadFile(Image=wxImage:new(), Name) of
        true ->
            E3d = wings_image:wxImage_to_e3d(Image),
            wxImage:destroy(Image),
            wxLogNull:destroy(BlockWxMsgs),
            e3d_image:fix_outtype(Name, E3d, Prop);
        false ->
            wxLogNull:destroy(BlockWxMsgs),
            {error, ignore}
    end.

write_image(Prop) ->
    Name  = proplists:get_value(filename, Prop),
    Image = proplists:get_value(image, Prop),
    case wxImage:saveFile(Wx = wings_image:e3d_to_wxImage(Image), Name) of
        true -> wxImage:destroy(Wx), ok;
        false -> wxImage:destroy(Wx), {error, ignore}
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
