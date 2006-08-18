%%
%%  wp8_qt_file.erl --
%%
%%     Native file dialog boxes for QT/Unix.
%%
%%  Copyright (c) 2001-2002 Patrik Nyblom, Bjorn Gustavsson
%%
%%  Changes for QT/Unix support by Chris Osgood : 2001/12/14
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wp8_qt_file).

-export([menus/0, init/1]).

%% Operations supported by driver.
-define(OP_QUESTION, 0).
-define(OP_READ, 1).
-define(OP_WRITE, 2).
-define(OP_MESSAGE, 3).

menus() ->
    [].
init(Next) ->
    case os:type() of
	{unix,_} ->
	    Dir = filename:dirname(code:which(?MODULE)),
	    case erl_ddll:load_driver(Dir, "qt_wings_file_drv") of
		ok ->
		    case open_port({spawn,"qt_wings_file_drv"},[]) of
			Port when is_port(Port) ->
			    register(wp8_file_port, Port),
			    fun(What) ->
				    fileop(What,Next)
			    end;
			Other ->
			    Next
		    end;
		Else ->
		    Next
	    end;
	_ ->
	    Next
    end.

fileop({question,Question}, _Next) ->
    list_to_atom(erlang:port_control(wp8_file_port, ?OP_QUESTION,
				     ["Wings 3D",0,Question,0]));
fileop({message,Message}, _Next) ->
    Title = "Wings 3D",
    erlang:port_control(wp8_file_port, ?OP_MESSAGE, [Title,0,Message,0]);
fileop({file,open_dialog,Prop}, _Next) ->
    Title = proplists:get_value(title, Prop, "Open"),
    file_dialog(?OP_READ, Prop, Title);
fileop({file,save_dialog,Prop}, _Next) ->
    Title = proplists:get_value(title, Prop, "Save"),
    file_dialog(?OP_WRITE, Prop, Title);
fileop(What, Next) ->
    Next(What).

file_dialog(Type, Prop, Title) ->
    Dir = wings_pref:get_value(current_directory),
    DefName = proplists:get_value(default_filename, Prop, ""),
    {ok,Cwd} = file:get_cwd(),
    file:set_cwd(Dir),
    Filters = file_filters(Prop),
    Data = [Dir,0,Title,0,DefName,0|Filters],
    case erlang:port_control(wp8_file_port, Type, Data) of
	[] ->
	    file:set_cwd(Cwd),
	    aborted;
	Else ->
	    file:set_cwd(Cwd),
	    filename:absname(Else) % Happens to turn windows slashes...
    end.

file_filters(Prop) ->
    Exts = case proplists:get_value(extensions, Prop, none) of
	       none ->
		   Ext = proplists:get_value(ext, Prop, ".wings"),
		   ExtDesc = proplists:get_value(ext_desc, Prop,
						 "Wings File"),
		   [{Ext,ExtDesc}];
	       Other -> Other
	   end,
    [file_add_all(Exts),file_filters_1(Exts++[{".*","All Files"}], [])].

file_filters_1([{Ext,Desc}|T], Acc0) ->
    Wildcard = "*" ++ Ext,
    Acc = [Acc0,Desc," (",Wildcard,")",0],
    file_filters_1(T, Acc);
file_filters_1([], Acc) -> [Acc,0].
    
file_add_all([_]) -> [];
file_add_all(Exts) ->
    All0 = ["*"++E || {E,_} <- Exts],
    All = file_add_semicolons(All0),
    ["All Formats (",All,")",0,All,0].

file_add_semicolons([E1|[E2|_]=T]) ->
    [E1,";"|file_add_semicolons(T)];
file_add_semicolons(Other) -> Other.
