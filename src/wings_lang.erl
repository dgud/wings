%%
%%  wings_lang.erl --
%%
%%     Implementation of languages.
%%
%%  Copyright (c) 2004 Riccardo Venier, Dan Gudmundsson
%%                2004-2005 Dan Gudmundsson, Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wings_lang.erl,v 1.21 2006/08/07 01:41:17 antoneos Exp $
%%
%%  Totally rewritten but Riccardo is still the one who did the hard work.
%%

-module(wings_lang).

%% Wings API.
-export([init/0,str/2,
	 available_languages/0,load_language/1]).

-import(lists, [reverse/1,reverse/2,foreach/2,foldl/3]).

-define(DEF_LANG_STR, "en").			% English
-define(DEF_LANG_ATOM, en).			% English
-define(LANG_DIRS, ["ebin","src","plugins","plugins_src"]).

str({_,_,_}=Key, DefStr) ->
    case get(?MODULE) of
	?DEF_LANG_ATOM -> DefStr;
	_ ->
	    try ets:lookup_element(?MODULE, Key, 2) of
		Str when is_binary(Str) -> binary_to_list(Str);
		Str when is_list(Str) -> Str
	    catch
		error:_ -> DefStr
	    end
    end.

init() ->
    wings_pref:set_default(language, ?DEF_LANG_STR),
    Lang = case wings_pref:get_value(language) of 
	       ?DEF_LANG_STR=L -> L;
	       Other ->
		   case lists:member(Other, available_languages()) of
		       true -> Other;
		       false -> ?DEF_LANG_STR
		   end
	   end,
    load_language_only(Lang).

load_language(Lang) when is_list(Lang) ->
    load_language_only(Lang),
    wings:init_menubar(),
    foreach(fun(W) -> wings_wm:send(W, language_changed) end,
	    wings_wm:windows()).

load_language_only(Lang0) when is_list(Lang0) ->
    Lang = list_to_atom(Lang0),
    put(?MODULE, Lang), 
    catch ets:delete(?MODULE), 
    case Lang of
	?DEF_LANG_ATOM -> ok;
	_  ->
	    ets:new(?MODULE, [named_table]),
	    Root = code:lib_dir(wings),
	    LangFile = "_"++Lang0++".lang",
	    load_language(Root, ?LANG_DIRS, LangFile)
    end.

load_language(_, [],_) -> ok;
load_language(Root, [Dir|Dirs], Lang) ->
    Path = filename:join(Root, Dir),
    case file:list_dir(Path) of
	{ok,List} ->
	    load_language_2(Path, List, Lang),
	    load_language(Root,Dirs,Lang);
	_ -> 
	    load_language(Root,Dirs, Lang)
    end.

load_language_2(Dir, [File|Fs], Lang) ->
    case catch lists:nthtail(length(File)-length(Lang), File) of
	Lang ->
	    load_language_file(filename:join(Dir, File));
	_ ->
	    Path = filename:join(Dir,File),
	    case filelib:is_dir(Path) of
		true ->
		    load_language(Dir, [File], Lang);
		false ->
		    ignore
	    end
    end,
    load_language_2(Dir, Fs, Lang);
load_language_2(_, [], _) -> ok.

load_language_file(File) ->
    case file:consult(File) of
	{ok,Terms} ->
	    io:format("Loading ~s\n", [File]),
	    load_file(Terms);
	{error,{Line,Mod,Info}} ->
	    io:format("~s, line ~p: ~s\n",
		      [File,Line,Mod:format_error(Info)]);
	Other ->
	    io:format("Problem reading language file ~p:\n~p\n",
		      [File,Other])
    end.

load_file([utf8|Trans]) ->
    load_file_1(utf8, Trans);
load_file([latin1|Trans]) ->
    load_file_1(latin1, Trans);
load_file(['iso-8859-1'|Trans]) ->
    load_file_1(latin1, Trans);
load_file([Atom|Trans]) when is_atom(Trans) ->
    io:format("Ignoring: ~p\n", [Atom]),
    load_file_1(default, Trans);
load_file(Trans) -> load_file_1(default, Trans).

load_file_1(latin1, Trans) ->
    %% ISO-8859-1/Latin 1: Just load the file without any transformation.
    io:format("  encoding: latin1\n"),
    load_file_2(Trans);
load_file_1(utf8, Trans0) ->
    %% UTF8: Transform the file; abort loading if any failures.
    case expand_utf8(Trans0) of
	Trans when is_list(Trans) ->
	    io:format("  encoding: utf8\n"),
	    load_file_2(Trans);
	{error,{Mod,Name,Key}} ->
	    %% Abort loading.
	    io:format("Bad UTF-8 string for module=~p, function=~p, key=~p\n",
		      [Mod,Name,Key])
    end;
load_file_1(default, Trans0) ->
    %% Default: Try UTF8 transformation first, fall back to no transformation
    %% if it fails.
    case expand_utf8(Trans0) of
	Trans when is_list(Trans) ->
	    io:format("  encoding: utf8 or plain US-ASCII\n"),
	    load_file_2(Trans);
	{error,_} ->
	    %% Just load it.
	    io:format("  encoding: probably latin1\n"),
	    load_file_2(Trans0)
    end.
    
load_file_2(Trans) ->
    Add = fun(Level, Str0) -> 
		  Key = list_to_tuple(reverse(Level)),
		  Str = try
			    list_to_binary(Str0)
			catch
			    error:badarg -> Str0
			end,
		  ets:insert(?MODULE, {Key,Str}) 
	  end,
    Trav = 
	fun(Data = [Cont|_], Level, Trav) when is_tuple(Cont) ->
		Insert = fun({Key, Next}) ->
				 Trav(Next, [Key|Level], Trav)
			 end,
		lists:foreach(Insert, Data);
	   (Str, Level, _) ->
		Add(Level,Str)
	end,
    Trav(Trans,[],Trav).

available_languages() ->
    Root = code:lib_dir(wings),
    Files = 
	filelib:wildcard(filename:join([Root,"src","*.lang"])) ++ 
	filelib:wildcard(filename:join([Root,"ebin","*.lang"])),
    Extract = fun(File, Acc) ->
		      BaseName = filename:basename(File),
		      case string:tokens(filename:rootname(BaseName),[$_]) of
			  [_Name,Lang] -> [Lang|Acc];
			  _ -> Acc
		      end
	      end,
    lists:usort([?DEF_LANG_STR|lists:foldl(Extract, [], Files)]).

%%%
%%% Character transformation support.
%%%

expand_utf8(ModFs) ->
    try
	expand_utf8_0(ModFs)
    catch
	throw:{error,_}=E ->
	    E
    end.

expand_utf8_0([{Mod,Fs}|ModFs]) ->
    [{Mod,expand_utf8_1(Fs, Mod)}|expand_utf8_0(ModFs)];
expand_utf8_0([]) -> [].

expand_utf8_1([{Name,Ss}|T], Mod) ->
    [{Name,expand_utf8_2(Ss, Mod, Name)}|expand_utf8_1(T, Mod)];
expand_utf8_1([], _) -> [].

expand_utf8_2([{Key,Str0}|T], Mod, Name) ->
    case wings_util:expand_utf8(Str0) of
	{Str,0} ->
	    [{Key,Str}|expand_utf8_2(T, Mod, Name)];
	{_,_} ->
	    %% Errors in the string.
	    throw({error,{Mod,Name,Key}})
    end;
expand_utf8_2([], _, _) -> [].
