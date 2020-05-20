%%
%%  user_default.erl --
%%
%%     Extends the Erlang shell with Wings utilities.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(user_default).

-export([help/0,wh/0,
	 wx/0,wxe/0,wxu/1,wxu/3,wxunref/0,wxundef/0,wxcs/0,
	 wxc/1,wxq/1,
	 dialyze/0,dialyze/1,
	 wldiff/1]).
-export([diana/0]).

-import(lists, [foldl/3,foreach/2,flatmap/2]).

help() ->
    shell_default:help(),
    p("** Other good stuff **\n"),
    p("mm()       -- lists modified modules on disk not loaded\n"),
    p("lm()       -- reload all modules that newer on disk\n"),
    p("** Wings commands **\n"),
    p("wh()       -- print help for Wings\n"),
    ok.

wh() ->
    p("dialyze() -- dialyze Wings\n"),
    p("diana()   -- show the result of the latest analysis again\n"),
    p("** Xref for Wings modules **\n"),
    p("wx()       -- collect xref information\n"),
    p("wxe()      -- add xref information for wx\n"),
    p("wxunref()  -- print unused functions\n"),
    p("wxundef()  -- print calls to undefined functions\n"),
    p("wxu(M)     -- print uses of module M\n"),
    p("wxu(M, F, A) -- print uses of M:F/A\n"),
    p("wxc(M)     -- print modules that M calls\n"),
    p("wxcs()     -- print strong components\n"),
    p("wxq(Query) -- execute an XREF query\n"),
    p("** Language support **\n"),
    p("wldiff(Lang) -- diff language files against English templates\n"),
    ok.

%%%
%%% Xref support.
%%%

wx() ->
    WingsLib = wings_util:lib_dir(wings),
    WingsEbin = filename:join(WingsLib, "ebin"),
    case whereis(s) of
	undefined -> ok;
	_ -> xref:stop(s)
    end,
    xref:start(s),
    xref:set_default(s, [{verbose,false},{warnings,false},{builtins,true}]),
    xref:set_library_path(s, code:get_path() -- [WingsEbin]),
    {ok,Ms} = xref:add_directory(s, WingsEbin),
    Dirs = get_plugin_dirs(),
    N = foldl(fun(D, N) ->
		      {ok,PMs} = xref:add_directory(s, D),
		      N+length(PMs)
	      end, length(Ms), Dirs),
    xref:q(s, "Wings := \"wings.*\":Mod + \"wp.*\":Mod + \"auv_.*\":Mod "),
    io:put_chars(" Variable Wings = <all modules in Wings>\n"),
    io:format(" Modules loaded: ~p\n", [N]),
    wxundef().

wxe() ->
    Dir = filename:dirname(code:which(gl)),
    {ok,Ms} = xref:add_directory(s, Dir),
    length(Ms).

wxu(Mod) when is_atom(Mod) ->
    wxq(make_query("domain(strict(ME || ~p))", [Mod]));
wxu({M,_,_}=MFA) ->
    wxq(make_query("domain(E || ~p) - ~p", [MFA,M])).

wxu(M, F, A) ->
    wxu({M,F,A}).

wxc(Mod) when is_atom(Mod) ->
    wxq(make_query("range(strict(ME|~p))*Wings", [Mod])).

wxcs() ->
    print_components(xref:q(s, make_query("components ME", []))).

wxq(Q) ->
    result(xref:q(s, Q)).

print_components({ok,Cs}) ->
    foreach(fun(C) -> io:format("~p\n", [C]) end, Cs);
print_components(Other) -> Other.

wxundef() ->
    case xref:analyze(s, undefined_function_calls) of
	{ok,Undef} -> Undef;
	Other -> Other
    end.

wxunref() ->
    {ok,Unref0} = xref:analyze(s, exports_not_used),
    Unref = filter_unref(Unref0),
    io:format("~p\n", [Unref]).

filter_unref([{M,F,A}=MFA|T]) ->
    case filter_unref(M, F, A) of
	true -> [MFA|filter_unref(T)];
	false -> filter_unref(T)
    end;
filter_unref([]) -> [].

filter_unref(user_default, _, _) -> false;
filter_unref(wings_start, start, 0) -> false;
filter_unref(wings_start, start, 1) -> false;
filter_unref(wings_start, start_halt, 0) -> false;
filter_unref(wings_start, start_halt, 1) -> false;
filter_unref(_, init, 1) -> false;
filter_unref(_, handle_event, 2) -> false;
filter_unref(_, handle_sync_event, 3) -> false;
filter_unref(_, handle_call, 3) -> false;
filter_unref(_, handle_cast, 2) -> false;
filter_unref(_, handle_info,2) -> false;
filter_unref(_, code_change, 3) -> false;
filter_unref(_, terminate, 2) -> false;

filter_unref(M, F, A) ->
    case atom_to_list(M) of
	"wpc_"++_ ->
	    filter_standard_plugin(F, A);
	"wp9_"++_ ->
	    filter_ui_plugin(F, A);
	"wpf_"++_ ->
	    filter_font_plugin(F, A);
	_ ->
	    true
    end.

filter_standard_plugin(init, 0) -> false;
filter_standard_plugin(command, 2) -> false;
filter_standard_plugin(menu, 2) -> false;
filter_standard_plugin(_, _) -> true.

filter_ui_plugin(init, 1) -> false;
filter_ui_plugin(_, _) -> true.

filter_font_plugin(char, 1) -> false;
filter_font_plugin(desc, 0) -> false;
filter_font_plugin(draw, 1) -> false;
filter_font_plugin(height, 0) -> false;
filter_font_plugin(width, 0) -> false;
filter_font_plugin(height, 1) -> false;
filter_font_plugin(width, 1) -> false;
filter_font_plugin(_, _) -> true.

result({ok,List}) ->
    io:format("~p\n", [List]);
result(Other) -> Other.

make_query(Format, Args) ->
    R = lists:flatten(io_lib:format(Format, Args)),
    %%io:format("~p\n", [R]),
    R.

%%%
%%% Dialyzer support.
%%%
dialyze() ->
    dialyze([core,plugins]).

dialyze(all) ->
    dialyze([core,wx,cl,plugins]);
dialyze(Atom) when is_atom(Atom) ->
    dialyze([Atom]);
dialyze(Dirs0) when is_list(Dirs0) ->
    Dirs = flatmap(fun(core) ->
			   WingsLib = code:lib_dir(wings),
			   [filename:join(WingsLib, "ebin")];
		      (wx) ->
			   [filename:dirname(code:which(wx))];
		      (cl) ->
			   [filename:dirname(code:which(cl))];
		      (plugins) ->
			   get_plugin_dirs()
		   end, Dirs0),
    case dialyzer:run([{files,Dirs}, {warnings, [no_improper_lists]}]) of
	{ok,Ws} -> dialyze_1(Ws);
	{ok,Ws,_} -> dialyze_1(Ws);
	Other -> dialyze_1(Other)
    end.

dialyze_1(Ws) ->
    File = dump_file(),
    file:write_file(File, term_to_binary(Ws)),
    diana().

diana() ->
    {ok,B} = file:read_file(dump_file()),
    case binary_to_term(B) of
	Ws when is_list(Ws) ->
	    diana_1(lists:keysort(2, Ws))
    end.

diana_1([W|Ws]) ->
    io:format("~s", [dialyzer:format_warning(W)]),
    diana_1(Ws);
diana_1([]) -> ok.

dump_file() ->
    filename:join(code:lib_dir(wings), "dialyzer_warnings.raw").

%%%
%%% Language support.
%%%

wldiff(Lang) when is_list(Lang) ->
    case filelib:wildcard("*_"++Lang++".lang") of
	[] ->
	    io:format("No ~p language files found.\n", [Lang]),
	    error;
	Files -> wldiff_1(Files)
    end;
wldiff(Lang) when is_atom(Lang) ->
    wldiff(atom_to_list(Lang)).

wldiff_1([F|Fs]) ->
    io:format("Diffing ~p (with English)\n", [F]),
    tools:diff(F),
    wldiff_1(Fs);
wldiff_1([]) -> ok.


%%%
%%% Internal functions.
%%%

p(String) ->
    io:put_chars(String).

get_plugin_dirs() ->
    Prefix = filename:join(wings_util:lib_dir(wings), "plugins"),
    Path = code:get_path(),
    get_plugin_dirs(Path, Prefix, []).

get_plugin_dirs([D|Ds], Prefix, Acc) ->
    case lists:prefix(Prefix, D) of
	false -> get_plugin_dirs(Ds, Prefix, Acc);
	true -> get_plugin_dirs(Ds, Prefix, [D|Acc])
    end;
get_plugin_dirs([], _, Acc) -> Acc.
