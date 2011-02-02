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

-include("wings_intl.hrl").

init(Next) ->
    wpa:pref_set_default(?MODULE, utf8, true),
    fun(What) -> ui(What, Next) end.

ui({file,open_dialog,Prop,Cont}, _Next) ->
    Title = proplists:get_value(title, Prop, ?__(1,"Open")),
    open_dialog(Title, Prop, Cont);
ui({file,save_dialog,Prop,Cont}, _Next) ->
    Title = proplists:get_value(title, Prop, ?__(2,"Save")),
    save_dialog(Title, Prop, Cont);
ui({image,formats,Formats}, _Next) ->
    image_formats(Formats);
ui({image,read,Prop}, _Next) ->
    read_image(Prop);
ui({image,write,Prop}, _Next) ->
    write_image(Prop);
ui(What, Next) -> Next(What).

read_image(Prop) ->
    Name = proplists:get_value(filename, Prop),
    e3d_image:load(Name, Prop).

write_image(Prop) ->
    Name  = proplists:get_value(filename, Prop),
    Image = proplists:get_value(image, Prop),
    e3d_image:save(Image, Name, Prop).

image_formats(Fs0) ->
    Fs1 = [{".bmp",?__(1,"BMP Bitmap File")},
	   {".tif",?__(2,"Tiff Bitmap")},
	   {".png",?__(3,"PNG File")},
	   {".tga",?__(4,"Targa File")}|Fs0],
    Fs2 = sofs:relation(Fs1),
    Fs3 = sofs:relation_to_family(Fs2),
    Fs = sofs:to_external(Fs3),
    [{Ext,Desc} || {Ext,[Desc|_]} <- Fs].

%%%
%%% File dialogs implemented using the dialog handler.
%%%

open_dialog(Title, Props, Cont) ->
    dialog(open, Title, Props, Cont).

save_dialog(Title, Props, Cont) ->
    dialog(save, Title, Props, Cont).

dialog(Type, Title, Props, Cont) ->
    [{_,Def}|_] = Types = file_filters(Props),
    Dir = proplists:get_value(directory, Props, "/"),
    Filename = proplists:get_value(default_filename, Props, ""),
    Ps = [{directory,Dir},{filetype,Def},{filename,Filename}],
    {dialog,Qs,Ask} = dialog_1(Type, Types, Title, Cont, Ps),
    wings_ask:dialog(Title, Qs, Ask).

dialog_1(DlgType, Types, Title, Cont, Ps) ->
    Dir = proplists:get_value(directory, Ps),
    DefType = proplists:get_value(filetype, Ps),
    Utf8 = wpa:pref_get(?MODULE, utf8),
    Filename0 = proplists:get_value(filename, Ps),
    Filename = maybe_pretty(Filename0, Utf8),
    Wc = atom_to_list(DefType),
    FileList = file_list(Dir, Wc, Utf8),
    DirMenu = dir_menu(Dir, Utf8, []),
    OkHook = fun(Op, Arg) -> ok_hook(Op, Arg, DlgType) end,
    Charset = case Utf8 of
		  false -> latin1;
		  true -> unicode
	      end,
    Qs = {vframe,
	  [{hframe,[{label,?__(1,"Look in")},
		    {menu,DirMenu,Dir,[{key,directory},{hook,fun menu_hook/2}]},
		    {button,?__(2,"Up"),fun(_) -> ignore end,
		     [{key,up},{hook,fun up_button/2}]}]},
	   panel,
	   FileList,
	   {?__(9,"UTF-8 encoded file names"),Utf8,
	    [{key,utf8},{hook,fun utf8_hook/2}]},
	   panel,
	   {hframe,
	    [{vframe,
	      [{label,?__(3,"File name")},
	       {label,?__(4,"File format")}]},
	     {vframe,
	      [{text,Filename,[{key,filename},{hook,fun filename_hook/2},
			       {charset,Charset}]},
	       {menu,Types,DefType,[{key,filetype},{hook,fun menu_hook/2}]}]},
	     {vframe,[{button,Title,
		       %% We will always exit the dialog through this
		       %% callback. The hook for this button will make
		       %% sure we don't get here until there is a valid
		       %% filename in the filename field.
		       fun(Res) ->
			       ok_action(Cont, Res)
		       end,[ok,{hook,OkHook}]},
		      {button,?__(5,"Cancel"),cancel,[cancel]}]}]}]},
    Ask = fun(Res0) ->
		  %% This callback is mainly used for restarting the dialog.
		  case proplists:get_value(filename, Res0) of
		      {NewFilename} ->
			  %% Here we must ask whether an existing
			  %% file should be overwritten.
			  Res1 = [{filename,NewFilename}|Res0],
			  YesNoQs = {vframe,
				     [{label,NewFilename ++
				       ?__(6," exists; overwrite?"),
				       [{break,45*wings_text:width()}]},
				      {hframe,
				       [{button,?__(7,"Yes"),
					 fun(_) ->
						 ok_action(Cont, Res1)
					 end},
					{button,?__(8,"No"),done,[cancel]}]}]},
			  {dialog,YesNoQs,
			   fun(_) ->
				   %% Will be called if the answer is No.
				   %% Restart the file dialog.
				   Name = maybe_unpretty(NewFilename, Utf8),
				   Res = [{filename,Name}|Res1],
				   dialog_1(DlgType, Types, Title, Cont, Res)
			   end};
		      Name0 ->
			  %% Standard restart.
			  Name = maybe_unpretty(Name0, Utf8),
			  Res = [{filename,Name}|Res0],
			  dialog_1(DlgType, Types, Title, Cont, Res)
		  end
	  end,
    {dialog,Qs,Ask}.

ok_action(Cont, Res) ->
    Dir = proplists:get_value(directory, Res),
    Name0 = proplists:get_value(filename, Res),
    Utf8 = proplists:get_value(utf8, Res),
    wpa:pref_set(?MODULE, utf8, Utf8),
    Name = case Utf8 of
	       false ->
		   Name0;
	       true ->
		   Name1 = unicode:characters_to_binary(Name0, utf8),
		   binary_to_list(Name1)
	   end,
    NewName = filename:join(Dir, Name),
    Cont(NewName).

ok_hook(is_disabled, {_Var,_I,Store}, _) ->
    %% The button will be disabled unless there is a filename
    %% in the filename field OR a directory is selected in
    %% file_list table.

    gb_trees:get(filename, Store) =:= [] andalso
	begin
	    case gb_trees:get(file_list, Store) of
		{[Sel],Els} ->
		    case lists:nth(Sel+1, Els) of
			{{{dir,_},_}} -> false;
			_ -> true
		    end;
		_ -> true
	    end
	end;
ok_hook(update, {_Var,_I,_Val,Store}, DlgType) ->
    %% The button was pressed. If a directory is selected
    %% in the file_list table, we'll update the current
    %% directory and force a restart of the dialog.
    %%    Otherwise, we'll check the filename before returning
    %% void (to finish the dialog).

    case gb_trees:get(file_list, Store) of
	{[Sel],Els} ->
	    case lists:nth(Sel+1, Els) of
		{{{dir,Dir0},_}} ->
		    Base = gb_trees:get(directory, Store),
		    Dir = filename:join(Base, Dir0),
		    {done,gb_trees:update(directory, Dir, Store)};
		_ ->
		    check_filename(Store, DlgType)
	    end;
	_ ->
	    check_filename(Store, DlgType)
    end;
ok_hook(_, _, _) -> void.

utf8_hook(update, {Var,_I,Val,Sto0}) ->
    Sto = gb_trees:update(filename, "", Sto0),
    wpa:pref_set(?MODULE, Var, Val),
    {done,gb_trees:update(Var, Val, Sto)};
utf8_hook(_, _) -> void.

filename_hook(update, {Var,_I,Val,Sto0}) ->
    {_,Els} = gb_trees:get(file_list, Sto0),
    Sto = gb_trees:update(file_list, {[],Els}, Sto0),
    {store,gb_trees:update(Var, Val, Sto)};
filename_hook(_, _) -> void.

check_filename(Store, DlgType) ->
    Utf8 = gb_trees:get(utf8, Store),
    Dir = gb_trees:get(directory, Store),
    Name0 = gb_trees:get(filename, Store),
    Name1 = maybe_add_extension(Name0, gb_trees:get(filetype, Store)),
    Name2 = maybe_unpretty(Name1, Utf8),
    Name = filename:join(Dir, Name2),
    case DlgType of
	open ->
	    case filelib:is_file(Name) of
		false ->
		    wings_u:message(Name0 ++ ?__(1," does not exist")),
		    done;
		true ->
		    {store,gb_trees:update(filename, Name1, Store)}
	    end;
	save ->
	    case filelib:is_file(Name) of
		false ->
		    {store,gb_trees:update(filename, Name1, Store)};
		true ->
		    %% Mark the filename to signal that an overwrite
		    %% question should be asked.
		    {done,gb_trees:update(filename, {Name1}, Store)}
	    end
    end.

dir_menu(Dir0, Utf8, Acc) ->
    Entry = case Dir0 of
		"/" ->
		    {Dir0,Dir0};
		_ ->
		    DirText0 = filename:basename(Dir0),
		    DirText = maybe_pretty(DirText0, Utf8),
		    {DirText,Dir0}
	    end,
    case filename:dirname(Dir0) of
	Dir0 -> [Entry|Acc];
	Dir -> dir_menu(Dir, Utf8, [Entry|Acc])
    end.

menu_hook(update, {Var,_I,Val,Sto}) ->
    {done,gb_trees:update(Var, Val, Sto)};
menu_hook(_, _) -> void.

up_button(update, {Var,_I,Val,Sto0}) ->
    Dir0 = gb_trees:get(directory, Sto0),
    Dir = filename:dirname(Dir0),
    Sto1 = gb_trees:update(directory, Dir, Sto0),
    Sto = gb_trees:update(Var, Val, Sto1),
    {done,Sto};
up_button(_, _) -> void.

file_filters(Prop) ->
    Exts = case proplists:get_value(extensions, Prop, none) of
	       none ->
		   Ext = proplists:get_value(ext, Prop, ".wings"),
		   ExtDesc = proplists:get_value(ext_desc, Prop,  ?__(1,"Wings File")),
		   [{Ext,ExtDesc}];
	       Other -> Other
	   end,
    [file_filter(Es) || Es <- Exts] ++ [{?__(2,"All Files (*)"),''}].

file_filter({"."++Ext0,Desc0}) ->
    Ext = list_to_atom(Ext0),
    Desc = Desc0 ++ " (*." ++ Ext0 ++ ")",
    {Desc,Ext}.

file_list(Dir, Wc, Utf8) ->
    {ok,Files0} = file:list_dir(Dir),
    {Folders,Files} = file_list_filter(Files0, Dir, Wc, Utf8),
    All0 = sort(Folders) ++ sort(Files),
    All = [{F} || F <- All0],
    {table,[{?__(1,"Filename")}|All],[{key,file_list},{hook,fun choose_file/2}]}.

file_list_filter(Files0, Dir, Wc, Utf8) ->
    {Folders,Files} = file_list_folders(Files0, Dir, Utf8, [], []),
    {Folders,file_list_filter_1(Files, Wc, Utf8)}.

file_list_filter_1(Files, [], Utf8) ->
    Space = {space,wings_text:width([folder])},
    [{F,[Space|maybe_pretty(F, Utf8)]} || F <- Files];
file_list_filter_1(Files, Wc, Utf8) ->
    Space = {space,wings_text:width([folder])},
    Ext = [$.|Wc],
    [{F,[Space|maybe_pretty(F, Utf8)]} ||
	F <- Files, lists:suffix(Ext, F)].

file_list_folders(["."++_|Fs], Dir, Utf8, DirAcc, FileAcc) ->
    file_list_folders(Fs, Dir, Utf8, DirAcc, FileAcc);
file_list_folders([F|Fs], Dir, Utf8, DirAcc, FileAcc) ->
    case filelib:is_dir(filename:join(Dir, F)) of
	true ->
	    DirItem = {{dir,F},[folder|maybe_pretty(F, Utf8)]},
	    file_list_folders(Fs, Dir, Utf8, [DirItem|DirAcc], FileAcc);
	false ->
	    file_list_folders(Fs, Dir, Utf8, DirAcc, [F|FileAcc])
    end;
file_list_folders([], _, _, DirAcc, FileAcc) -> {DirAcc,FileAcc}.

choose_file(update, {_Var,_I,{[],_},_Sto}) ->
    void;
choose_file(update, {Var,_I,{[Sel],Els}=Val,Sto0}) ->
    case lists:nth(Sel+1, Els) of
	{{{dir,_File},_}} ->
	    Sto = gb_trees:update(Var, Val, Sto0),
	    {store,Sto};
	{{File0,_}} ->
	    File = maybe_pretty(File0, gb_trees:get(utf8, Sto0)),
	    Sto1 = gb_trees:update(Var, Val, Sto0),
	    {store,gb_trees:update(filename, File, Sto1)}
    end;
choose_file(_, _) -> void.

maybe_unpretty(Name0, true) ->
    Name = unicode:characters_to_binary(Name0, utf8),
    binary_to_list(Name);
maybe_unpretty(Name, false) -> Name.

maybe_pretty(Name, true) -> wings_u:pretty_filename(Name);
maybe_pretty(Name, false) -> Name.

maybe_add_extension(Name, '') -> Name;
maybe_add_extension(Name, Ext) ->
    ensure_extension(Name, ["."++atom_to_list(Ext)]).

ensure_extension(Name, [Ext]) ->
    case eq_extensions(Ext, filename:extension(Name)) of
	true -> Name;
	false -> Name ++ Ext
    end;
ensure_extension(Name, [_|_]) -> Name.

eq_extensions(Ext, Actual) when length(Ext) =/= length(Actual) ->
    false;
eq_extensions(Ext, Actual) ->
    IgnoreCase = case os:type() of
		     {win32,_} -> true;
		     _ -> false
		 end,
    eq_extensions(Ext, Actual, IgnoreCase).

eq_extensions([C|T1], [C|T2], _IgnoreCase) ->
    eq_extensions(T1, T2);
eq_extensions([L|T1], [C|T2], true) when $A =< C, C =< $Z, L-C =:= 32 ->
    eq_extensions(T1, T2);
eq_extensions([_|_], [_|_], _IgnoreCase) -> false;
eq_extensions([], [], _IgnoreCase) -> true.
