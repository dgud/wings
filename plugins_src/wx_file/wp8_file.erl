%%
%%  wp8_file.erl --
%%
%%     Native file dialog boxes for Win32.
%%
%%  Copyright (c) 2001 Patrik Nyblom
%%                2002-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wp8_file).

-export([menus/0, init/1]).

-include_lib("wx/include/wx.hrl").  %% includes wx headers
-include("wings_intl.hrl").

%% Operations supported by driver.
menus() -> [].

init(Next) ->
    case get(top_frame) of
	undefined -> Next;
	_ ->
	    fun(What) -> fileop(What,Next) end
    end.

fileop({file,open_dialog,Prop,Cont}, _Next) ->
    Title = proplists:get_value(title, Prop, ?__(1,"Open")),
    file_dialog(?wxFD_OPEN, Prop, Title, Cont);
fileop({file,font_dialog,Prop,Cont}, Next) ->
    fileop({file,open_dialog,Prop,Cont}, Next);  % foward the call to open dialog
fileop({file,save_dialog,Prop,Cont}, _Next) ->
    Title = proplists:get_value(title, Prop, ?__(2,"Save")),
    file_dialog(?wxFD_SAVE, Prop, Title, Cont);
fileop(What, Next) ->
    Next(What).

file_dialog(Type, Prop, Title, Cont) ->
    Frame = get(top_frame),
    DefDir = proplists:get_value(directory, Prop),
    DefName = proplists:get_value(default_filename, Prop, ""),
    Filters = file_filters(Prop),
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

file_filters(Prop) ->
    Exts = case proplists:get_value(extensions, Prop, none) of
	       none ->
		   Ext = proplists:get_value(ext, Prop, ".wings"),
		   ExtDesc = proplists:get_value(ext_desc, Prop,
						 ?__(1,"Wings File")),
		   [{Ext,ExtDesc}];
	       Other -> Other
	   end,
    lists:flatten([file_add_all(Exts),
		   file_filters_0(Exts++[{".*", ?__(2,"All Files")}])]).

file_filters_0(Exts) ->
    file_filters_1(lists:reverse(Exts),[]).

file_filters_1([{Ext,Desc}|T], Acc) ->
    Wildcard = "*" ++ Ext,
    ExtString = [Desc," (",Wildcard,")","|",Wildcard|Acc],
    case T of
	[] -> ExtString;
	_  ->
	    file_filters_1(T, ["|"|ExtString])
    end.
    
file_add_all([_]) -> [];
file_add_all(Exts) ->
    All0 = ["*"++E || {E,_} <- Exts],
    All = file_add_semicolons(All0),
    [?__(1,"All Formats")++" (",All,")", "|", All].

file_add_semicolons([E1|[_|_]=T]) ->
    [E1,";"|file_add_semicolons(T)];
file_add_semicolons(Other) -> Other.

