#!/usr/bin/env escript
%%   -*- erlang -*-
%%     Wings 3D File convertion.
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-mode(compile).
-compile([{nowarn_deprecated_function, {erlang,get_stacktrace,0}}]).

%% If moved outside of wings directory modify
-define(WINGS_DIR, "c:/src/wings/ebin").

-record(opts,
	{dir = ".",       %% Ouput to directory
	 out_module,      %% Output format
	 verbose=false,   %% Verbose output
	 in_format,       %% In format (if unknown extension).
	 image_format,    %% Image out format
	 in_formats=[],   %% Scanned, all import formats
	 out_formats=[],  %% Scanned, all export formats
	 modify=[]        %% Convertion modifications
	}).

-record(format,
	{mod,             %% Module
	 ext_type,        %% Extension
	 str="",          %% Description string
	 option=false     %% Allows options
	}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(Args) ->
    Wings_dir = setup_paths(),
    ok = wings_start:start(script_usage),
    IEDir = filename:join(Wings_dir, "plugins/import_export"),
    code:add_patha(IEDir),
    Opts0 = scan_format([IEDir]),
    put(verbose, false),
    case parse_args(Args, Opts0) of
	{#opts{out_module=undefined},_} ->
	    io:format("**** Error:  Out format not specified~n~n"),
	    usage(Opts0);
	{Opts = #opts{}, Files} ->
	    convert(Files, Opts),
            quit(0);
	error ->
	    usage(Opts0)
    end.

convert(Fs, Opts) ->
    Convert = fun(File) ->
                      call_wings({file, confirmed_new}),
                      ok = import_file(File, Opts),
                      ok = do_mods(Opts),
                      Out = filename:rootname(filename:basename(File)),
                      export_file(Out, Opts)
              end,
    [Convert(File) || File <- Fs],
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scan_format(Dir) ->
    Files = filelib:wildcard(filename:join(Dir, "wpc_*.beam")),
    Plugin = fun(File, {Type, Acc}) ->
		     Mod = list_to_atom(filename:rootname(filename:basename(File))),
		     case Mod:menu({file, Type}, []) of
			 [{Str, ExtT, Extra}] ->
			     F = #format{mod=Mod, ext_type=ExtT, str = strip(Str),
					 option = lists:member(option, Extra)
					},
			     {Type,[F|Acc]};
			 [{Str, ExtT}] ->
			     F = #format{mod=Mod, ext_type=ExtT, str = strip(Str)},
			     {Type,[F|Acc]};
			 [] ->
			     {Type, Acc}
		     end
	     end,
    Default = [#format{mod=nendo, ext_type=ndo, str="Nendo (.ndo)"},
	       #format{mod=wings, ext_type=wings, str="Wings (.wings)"}],
    {_,Export} = lists:foldl(Plugin, {export, Default}, Files),
    {_,Import} = lists:foldl(Plugin, {import, Default}, Files),

    #opts{in_formats=Import, out_formats=Export}.

strip(Str) ->
    strip_1(lists:reverse(Str)).

strip_1([$.|Rest]) ->
    strip_1(Rest);
strip_1(Str) ->
    lists:reverse(Str).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_args(["-o", Dir|Rest], Opts) ->
    parse_args(Rest, Opts#opts{dir=Dir});
parse_args(["--outdir", Dir|Rest], Opts) ->
    parse_args(Rest, Opts#opts{dir=Dir});
parse_args(["-v"|Rest], Opts) ->
    put(verbose, true),
    parse_args(Rest, Opts#opts{verbose=true});
parse_args(["--verbose"|Rest], Opts) ->
    put(verbose, true),
    parse_args(Rest, Opts#opts{verbose=true});

parse_args(["--subdiv", N0|Rest], Opts=#opts{modify=Mod}) ->
    N = try
	    list_to_integer(N0)
	catch _:_ ->
		io:format("**** Error: Option --subdiv ~p Not an integer ~n~n", [N0]),
		usage(Opts)
	end,
    parse_args(Rest, Opts#opts{modify=[{subdivisions, N}|Mod]});
parse_args(["--tess"++_, "tri"++_|Rest], Opts=#opts{modify=Mod}) ->
    parse_args(Rest, Opts#opts{modify=[{tesselation,triangulate}|Mod]});
parse_args(["--tess"++_, "quad"++_|Rest], Opts=#opts{modify=Mod}) ->
    parse_args(Rest, Opts#opts{modify=[{tesselation,quadrangulate}|Mod]});
parse_args(["--name", Name|Rest], Opts=#opts{modify=Mod}) ->
    parse_args(Rest, Opts#opts{modify=[{exp_name,Name}|Mod]});

parse_args(["--swap_y_z"++_|Rest], Opts=#opts{modify=Mod}) ->
    parse_args(Rest, Opts#opts{modify=[{swap_y_z,true}|Mod]});
parse_args(["--scale", Scale|Rest], Opts=#opts{modify=Mod}) ->
    F = try list_to_float(Scale)
	catch _:_ ->
                try float(list_to_integer(Scale))
                catch _:_ ->
                        io:format("**** Error: Option --scale ~p not a  ~n~n", [Scale]),
                        usage(Opts)
                end
	end,
    parse_args(Rest, Opts#opts{modify=[{scale, F}|Mod]});
parse_args(["--uv", Bool|Rest], Opts=#opts{modify=Mod}) ->
    parse_args(Rest, Opts#opts{modify=[{include_uvs,bool(Bool, Opts)}|Mod]});
parse_args(["--n", Bool|Rest], Opts=#opts{modify=Mod}) ->
    parse_args(Rest, Opts#opts{modify=[{include_normals,bool(Bool, Opts)}|Mod]});
parse_args(["--image", Ext|Rest], Opts=#opts{modify=Mod}) ->
    parse_args(Rest, Opts#opts{modify=[{image,Ext}|Mod]});

parse_args(["--informat", Format|Rest], Opts) ->
    parse_args(Rest, Opts#opts{in_format=check_format(in, Format, Opts)});
parse_args(["-f", Format|Rest], Opts) ->
    parse_args(Rest, Opts#opts{out_module=check_format(out, Format, Opts)});
parse_args([Opt=[$-|_]| _], Opts) ->
    io:format("**** Error:  Unknown option ~p~n~n", [Opt]),
    usage(Opts);
parse_args(Files, Opts) ->
    {Opts, Files}.

check_format(Dir, Ext = [A|_], Opts) when A =/= $. ->
    check_format(Dir, [$.|Ext], Opts);
check_format(in, Ext, O=#opts{in_formats=In}) ->
    case get_module(Ext, In) of
	error ->
	    check_format_err(in, Ext, O);
	Mod -> Mod
    end;
check_format(out, Ext, O=#opts{out_formats=Out}) ->
    case get_module(Ext, Out) of
	error ->
	    check_format_err(out, Ext, O);
	Mod -> Mod
    end.

check_format_err(Dir, Format, Opts) ->
    io:format("**** Error:  Format ~p for ~pput is not supported ~n~n", [Format,Dir]),
    usage(Opts).

bool(Str, Opts) ->
    case string:lowercase(Str) of
        "false" -> false;
        "true" -> true;
        _ -> io:format("**** Error ~p is not a boolean ~n~n",[Str]),
             usage(Opts)
    end.

usage(#opts{in_formats=In, out_formats=Out}) ->
    io:format("Usage: wings_convert -f OutFormat [Opts] Files ~n"
	      "  Converts between file formats. ~n"
	      "  Output is written to the current directory by default.~n~n"
              "  Example: wings_convert -f obj --subdiv 1 --tess quad ../model.wings~n~n"
	      " Options:~n"
	      "   -o, --outdir DIR       Write converted files to DIR.~n"
	      "   -v, --verbose          Verbose output.~n"
	      "   --informat FORMAT      Ignore file extension and use FORMAT as input.~n"
	      "   --subdiv N             Subdivide object N times (default 0).~n"
	      "   --tess TYPE            Tesselate object none|tri|quad (default none)~n"
	      "   --name Name            Only Export Object with Name~n"
              "   --swap_y_z             Swap axis (if supported by exporter) (default false)~n"
              "   --scale Float          Scale (if supported by exporter) (default 1.0)~n"
              "   --uv Bool              Export uv (if supported by exporter) (default true)~n"
              "   --n Bool               Export normals (if supported by exporter) (default true)~n"
	      "   --image Ext            Convert images (if supported by exporter) (default .png)~n"
	      "~n"
	     ),
    io:format("~nSupported import formats:~n",[]),
    [io:format("  ~s~n", [Str]) || #format{str=Str} <- In],
    io:format("~nSupported export formats:~n",[]),
    [io:format("  ~s~n", [Str]) || #format{str=Str} <- Out],
    io:nl(),
    quit(1).

quit(Exit) ->
    verbose("Script: exiting\n", []),
    Ref = monitor(process, wings),
    wings ! {action, {file, confirmed_quit}},
    receive
        {'DOWN',Ref,_,_,_Reason} ->
            timer:sleep(100),
            halt(Exit)
    after 1000 ->
            halt(Exit)
    end.

get_module(Ext, [F=#format{str=Str}|List]) ->
    case string:str(Str,Ext) of
	0 ->
	    get_module(Ext,List);
	_ ->
	    F
    end;
get_module(_, []) ->
    error.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

call_wings(Cmd) ->
    verbose("Script: ~p~n",[{action, Cmd}]),
    wings ! {action, Cmd},
    Me = self(),
    Sync = fun(_St) ->
                   Me ! {cmd, sync},
                   keep
           end,
    wings ! {external, Sync},
    receive {cmd, sync} -> ok end,
    ok.

import_file(File, Opts) ->
    verbose("~s => ~n", [File]),
    wings ! {action, fun() -> put(wings_not_running, {import, File}), keep end},
    try import_file_1(File,Opts) of
	{error,Reason} ->
	    io:format("**** Import Failed: ~p On file: ~p~n~n", [Reason, File]),
	    quit(1);
	ok ->
            ok
    catch
	_:{command_error,Message} ->
	    io:format("**** Import Failed: ~s On file: ~p~n~n", [Message, File]),
	    quit(1);
	_:Reason ->
	    io:format("**** Import crashed: ~p On file: ~p~n~n", [Reason, File]),
	    io:format("Debug info: ~p~n~n",[erlang:get_stacktrace()]),
	    quit(1)
    end.

import_file_1(File, Opts=#opts{in_format=undefined}) ->
    import_file_2(filename:extension(File),File,Opts);

import_file_1(File, Opts=#opts{in_format=InFormat}) ->
    import_file_2(InFormat,File,Opts).

import_file_2(#format{mod=wings}, File, _) ->
    call_wings({file, {confirmed_open, File}});
import_file_2(#format{mod=nendo}, File, _) ->
    call_wings({file, {import, {ndo, File}}});
import_file_2(#format{ext_type=Type, option=false}, _File, _Opts)  ->
    call_wings({file,{import,Type}});
import_file_2(#format{ext_type=Type, option=true}, _File, _) ->
    call_wings({file,{import,{Type,[]}}});

import_file_2(Str, File, Opts = #opts{in_formats=In}) ->
    case get_module(Str, In) of
	error ->
	    io:format("**** Error:  Import Failed: ~p On file: ~p~n~n",
		      ["Unknown import format", File]),
	    quit(1);
	Mod -> import_file_2(Mod, File, Opts)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_mods(#opts{modify=Mods}) ->
    modify_model(Mods).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export_file(File, Opts=#opts{dir=Dir, out_module=F=#format{ext_type=Ext}}) ->
    FileName = filename:join(Dir, File++"."++ atom_to_list(Ext)),
    verbose("Export to: ~s~n", [FileName]),
    wings ! {action, fun() -> put(wings_not_running, {export, FileName}), keep end},

    try export_file_1(F, FileName, Opts) of
	{error,Reason} ->
	    io:format("**** Export Failed: ~p On file: ~p~n~n", [Reason, FileName]),
	    quit(1);
	ok ->
	    ok
    catch
	_:{command_error,Message} ->
	    io:format("**** Export Failed: ~s On file: ~p~n~n", [Message, File]),
	    quit(1);
	_:Reason ->
	    io:format("**** Export crashed: ~p On file: ~p~n~n", [Reason, FileName]),
	    io:format("Debug info: ~p~n~n",[erlang:get_stacktrace()]),
	    quit(1)
    end.

export_file_1(F, FileName, #opts{modify=Mods}) ->
    Opts = pick_export_opts(Mods),
    case proplists:get_value(exp_name, Mods, false) of
        false -> export_file_2(F, FileName, false, Opts);
        _Name -> export_file_2(F, FileName, true, Opts)
    end.

export_file_2(#format{mod=wings}, FileName, Selected, _) ->
    call_wings({file, {exp_cmd(save_as, Selected), {FileName, ignore}}});
export_file_2(#format{mod=nendo}, FileName, Selected, _) ->
    call_wings({file, {exp_cmd(export, Selected), {ndo, FileName}}});
export_file_2(#format{ext_type=Type, option=false}, _FN, Selected, _) ->
    call_wings({file,{exp_cmd(export, Selected),Type}});
export_file_2(#format{ext_type=Type, option=true}, _F, Selected, Opts) ->
    call_wings({file,{exp_cmd(export, Selected),{Type, Opts}}}).

exp_cmd(export, false) -> export;
exp_cmd(export, true) -> export_selected;
exp_cmd(save_as, false) -> save_as;
exp_cmd(save_as, true) -> save_selected.

pick_export_opts(Mods) ->
    [{include_uvs, proplists:get_value(include_uvs, Mods, true)},
     {include_normals, proplists:get_value(include_normals, Mods, true)},
     {swap_y_z, proplists:get_value(swap_y_z, Mods, false)},
     {export_scale, proplists:get_value(scale, Mods, 1.0)},
     {default_filetype, string:lowercase(proplists:get_value(image, Mods, ".png"))}
    ].

verbose(F,A) ->
    get(verbose) andalso io:format(F,A).

modify_model([]) ->
    ok;
modify_model(Ps) ->
    SubDivs = proplists:get_value(subdivisions, Ps, 0),
    Tess = proplists:get_value(tesselation, Ps, none),
    call_wings({select, body}),
    case proplists:get_value(exp_name, Ps, false) of
        false -> call_wings({select, all});
        Name -> call_wings({select, {by, {by_name_with, Name}}})
    end,
    sub_divide(SubDivs),
    tesselate(Tess),
    ok.

sub_divide(0) -> ok;
sub_divide(N) ->
    call_wings({body, smooth}),
    sub_divide(N-1).

tesselate(none) -> ok;
tesselate(triangulate) ->
    call_wings({tools,{virtual_mirror, freeze}}),
    call_wings({select, face}),
    call_wings({face, {tesselate, triangulate}});
tesselate(quadrangulate) ->
    call_wings({tools,{virtual_mirror, freeze}}),
    call_wings({select, face}),
    call_wings({face, {tesselate, quadrangulate}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%

setup_paths() ->
    Escript   = filename:dirname(filename:absname(escript:script_name())),
    Installed = installed(Escript),
    EnvDir    = os:getenv("WINGS_DIR"),
    DefDir    = ?WINGS_DIR,
    case test_paths([Installed, Escript,EnvDir,DefDir]) of
	{ok, Path} ->
	    code:add_patha(filename:join([Path, "ebin"])),
	    Path;
	_ ->
	    io:format("**** Error:  Compiled wings files not found~n~n"),
	    io:format("             use 'set WINGS_DIR=c:\PATH_TO_WINGS_INSTALL~n~n")
    end.

test_paths([false|Rest]) -> test_paths(Rest);
test_paths([Path0|Rest]) ->
    Path = strip_path(lists:reverse(Path0)),
    case filelib:is_regular(filename:join([Path, "ebin", "wings.beam"])) of
	true  -> {ok, Path};
	false -> test_paths(Rest)
    end;
test_paths([]) -> not_found.

strip_path("nibe/" ++ Path) -> lists:reverse(Path);
strip_path("crs/"  ++ Path) -> lists:reverse(Path);
strip_path(Path)            -> lists:reverse(Path).

installed(Path) ->
    Lib = filename:join(Path, "lib"),
    case filelib:wildcard("wings-*", Lib) of
        [WingsDir] ->
            filename:join(Lib, WingsDir);
        [] ->
            false;
        [_|_] = Strange ->
            io:format("Ignore bad installation (please report):~n ~p~n",
                      [Strange]),
            false
    end.
