%%
%%  wings_job.erl
%%
%%     OS job handling support for plugin writers.
%%
%%  Copyright (c) 2005-2011 Raimo Niskanen
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_job).

-export([browse_props/0,render_formats/0,init/0,command/2,
	 find_executable/1,export_done/1,render/5,
	 run/3,altname/1,quote/1,
	 timestr/1,timestr/2,uniqstr/0]).

-include_lib("kernel/include/file.hrl").
-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-include("wings.hrl").



%% {button,{text,...}} props flag
%%
browse_props() ->
    {props,[{dialog_type,open_dialog},{directory,"/"},
	    case os:type() of
		{win32,_} -> 
		    {extensions,[{".exe","Windows Executable"}]};
		_-> {extensions,[]}
	    end]}.

%% Render formats that may be handled
%%
render_formats() ->
    [{tga,".tga","Targa bitmap"},
     {bmp,".bmp","Bitmap"},
     {png,".png","Portable Network Graphics"},
     {jpg,".jpg","JPeg compressed bitmap"},
     {hdr,".hdr","High Dynamic Range image"},
     {exr,".exr","OpenEXR"}].
%% Formats that internal image loader can handle
%%
-define(LOAD_IMAGE_FORMATS, [tga,bmp,png,jpg]).

%%
%%
init() ->
    wings_pref:set_default(render_load_image, false),
    wings_pref:set_default(viewer_frame, 1),
    lists:foreach(
      fun({Format,_,_}) ->
	      wings_pref:set_default({viewer,Format}, ""),
	      wings_pref:set_default({viewer_preopts,Format}, ""),
	      wings_pref:set_default({viewer_postopts,Format}, "")
      end, render_formats()),
    ok.

    

%% Generic handling of rendering followup.
%%
command({render_done,Handler,Status}, St) ->
    render_done(Handler(Status), St).

render_done({error,Error}, _St) ->
    io:format("Rendering error: ~p~n~n", [Error]),
    wpa:error_msg("Rendering error");
render_done({Format,Filename}, _St) ->
    io:format("Rendering Job ready~n~n"),
    ViewImage = wings_pref:get_value(render_load_image), % Misleading prefname
    Viewer = wings_pref:get_value({viewer,Format}),
    case {ViewImage,Viewer} of
	{true,""} -> ok;
	{true,_} ->
	    %% External Viewer
	    io:format("Viewing rendered image~n~n"),
	    view_image(Filename, Format, Viewer); % Ignoring errors
	{false,_} -> ok
    end,
    case lists:member(Format, ?LOAD_IMAGE_FORMATS) of
	true ->
	    io:format("Loading rendered image~n~n"),
	    load_image(Filename);
	_ -> keep
    end.

load_image(Filename) ->
    case wpa:image_read([{filename,Filename},
			 {alignment,1}]) of
	#e3d_image{}=Image ->
	    Id = wings_image:new_temp("<<Rendered>>", Image),
	    case wings_pref:get_value(render_load_image) of
	      true -> keep;
	      false ->
	        wings_image:window(Id),
	        keep
	    end;
	_ ->
	    wpa:error_msg("No image rendered")
    end.

view_image(Filename, Format, Viewer) ->
    ViewerPreopts = wings_pref:get_value({viewer_preopts,Format}, ""),
    ViewerPostopts = wings_pref:get_value({viewer_postopts,Format}, ""),
    case filename:pathtype(Viewer) of
	absolute ->
	    case altname(Viewer) of
		{error,Reason}=Error ->
		    io:format("Viewing error: ~p~n~n", [Reason]),
		    Error;
		Altname ->
		    view_image_1(Filename, Altname, 
				 ViewerPreopts, ViewerPostopts)
	    end;
	_ ->
	    view_image_1(Filename, Viewer,
			 ViewerPreopts, ViewerPostopts)
    end.

view_image_1(Filename, Viewer, ViewerPreopts, ViewerPostopts) ->
    Dirname = filename:dirname(Filename),
    Basename = filename:basename(Filename),
    Cmd = quote(Viewer)
	++case ViewerPreopts of "" -> ""; _ -> " " end++ViewerPreopts
	++" "++quote(Basename)
	++case ViewerPostopts of "" -> ""; _ -> " " end++ViewerPostopts,
    Handler = 
	fun (Port) ->
		if is_port(Port) ->
			io:format("Viewer started ~p:~n"
				  "> ~s~n", [self(),Cmd]);
		   true ->
			io:format("Viewer start failed:~n"
				  "> ~s~n", [Cmd])
		end,
		fun (ExitStatus) ->
			io:format("Viewer returned: ~p~n", [ExitStatus])
		end
	end,
    run(Cmd, Handler, [{cd,Dirname}]).



%% Quite like os:find_executable, but if the found executable is a .bat file
%% on windows; scan the .bat file for a real executable file.
%%
find_executable(Name) when is_list(Name) ->
    case os:find_executable(Name) of
	false ->
	    false;
	Filename ->
	    case os:type() of
		{win32,_} ->
		    case lowercase(filename:extension(Filename)) of
			".bat" ->
			    find_in_bat(Filename);
			_ ->
			    Filename
		    end;
		_ ->
		    Filename
	    end
    end.

%% Search .bat file for an executable file
find_in_bat(Filename) ->
    case file:open(Filename, [read]) of
	{ok,F} ->
	    R = scan_bat(F),
	    file:close(F),
	    R;
	_ ->
	    false
    end.

%% Scan each line of the .bat file
scan_bat(F) ->
    case rskip_warg(skip_walpha(io:get_line(F, ""))) of
	"" ->
	    scan_bat(F);
	"echo"++[C|_] when C==$ ; C==$\t; C==$\n ->
	    scan_bat(F);
	"set"++[C|_] when C==$ ; C==$\t; C==$\n ->
	    scan_bat(F);
	Line when is_list(Line) ->
	    %% Check if this is the name of an executable file
	    File = [C || C <- Line, C =/= $"], % Remove doublequotes
	    Filename = filename:nativename(File),
	    case file:read_file_info(Filename) of
		{ok,#file_info{type=regular,access=A,mode=M}} ->
		    case A of
			read when (M band 8#500) == 8#500 ->
			    Filename;
			read_write when (M band 8#500) == 8#500 ->
			    Filename;
			_ ->
			    scan_bat(F)
		    end;
		_ ->
		    scan_bat(F)
	    end
    end.



%% Skip whitespace and one '@' from beginning of line
%%
skip_walpha([$ |T]) ->
    skip_walpha(T);
skip_walpha([$\t|T]) ->
    skip_walpha(T);
skip_walpha([$\n|T]) ->
    skip_walpha(T);
skip_walpha([$@|T]) ->
    skip_walpha1(T);
skip_walpha(L) ->
    L.
%%
skip_walpha1([$ |T]) ->
    skip_walpha(T);
skip_walpha1([$\t|T]) ->
    skip_walpha(T);
skip_walpha1([$\n|T]) ->
    skip_walpha(T);
skip_walpha1(L) ->
    L.

%% Skip whitespace and '%d' .bat file arguments from end of line
%%
rskip_warg(L) ->
    rskip_warg1(lists:reverse(L)).
%%
rskip_warg1([$ |T]) ->
    rskip_warg1(T);
rskip_warg1([$\t|T]) ->
    rskip_warg1(T);
rskip_warg1([$\n|T]) ->
    rskip_warg1(T);
rskip_warg1([D,$%|T]) when D >= $0, D =< $9 ->
    rskip_warg1(T);
rskip_warg1(L) ->
    lists:reverse(L).

%% Convert all A-Z in string to lowercase
%%
lowercase([]) ->
    [];
lowercase([C|T]) when C >= $A, C =< $Z ->
    [(C + $a - $A)|lowercase(T)];
lowercase([C|T]) ->
    [C|lowercase(T)].



%% High level export function, it just prints the 
%% elapsed times to match the render function printout.
%%
export_done(ExportTS) ->
    RenderTS = os:timestamp(),
    io:format("Export time:     ~s~n", 
	      [wings_job:timestr(RenderTS, ExportTS)]),
    ok.

%% High level render function. Handler is always called.
%%   Handler(ok)    -> {Format,Filename};
%%   Handler(Error) -> Error.
%%
render(ExportTS, Renderer, ArgStr, PortOpts, Handler) ->
    RenderTS = os:timestamp(),
       case altname(Renderer) of
	   {error,_}=Error ->
	       io:format("~nRenderer ~p not found: ~p~n", 
			 [Renderer,Error]),
	       Handler(Error);
	   Altname ->
	       Cmd = wings_job:quote(Altname)++" "++ArgStr,
	       DoneFun = 
		   fun (ExitStatus) ->
			   io:format("~nRendering Job returned: ~p~n", 
				     [ExitStatus]),
			   FinishTS = os:timestamp(),
			   io:format("Export time:     ~s~n"++
				     "Render time:     ~s~n"++
				     "Total time:      ~s~n",
				     [wings_job:timestr(RenderTS, 
							ExportTS),
				      wings_job:timestr(FinishTS, 
							RenderTS),
				      wings_job:timestr(FinishTS, 
							ExportTS)]),
			   Status =
			       case ExitStatus of
				   0                    -> ok;
				   N when is_integer(N) -> {error,N};
				   undefined            -> ok;
				   _                    -> ExitStatus
			       end,
			   wpa:send_command(
			     {?MODULE,{render_done,Handler,Status}})
		   end,
	       RenderFun = 
		   fun (Port) ->
			   if is_port(Port) ->
				   io:format("Rendering Job started ~p:~n"
					     "> ~s~n", [self(),Cmd]);
			      true ->
				   io:format("Rendering Job start failed:~n"
					     "> ~s~n", [Cmd])
			   end,
			   DoneFun
		   end,
	       wings_job:run(Cmd, RenderFun, PortOpts)
       end,
    ok.


%% Spawn_link a process to run an OS command with possibly [{cd,Dir}] 
%% port options. The Handler is called as Handler(Port) -> Done 
%% that is called as Done(JobResult) where JobResult is 'ok' | 
%% integer exit code |{error,Reason}, or as Handler({Class,Reason}) 
%% if the OS spawn of command Cmd failed.
%%
%% Returns Pid.
run(Cmd, Handler, PortOpts) 
  when is_list(Cmd), is_function(Handler), is_list(PortOpts) ->
    spawn_link(
       fun () ->
	       process_flag(trap_exit, true),
	       Opts = PortOpts++[eof,exit_status,stderr_to_stdout],
	       try open_port({spawn,Cmd}, Opts) of
		   Port ->
		       (Handler(Port))(job(Port))
	       catch 
		   Class:Reason ->
		       Error = {Class,Reason},
		       (Handler(Error))(Error)
	       end
       end).

job(Port) ->
    receive
	{Port,eof} ->
	    receive 
		{Port,{exit_status,ExitStatus}} ->
		    ExitStatus
	    after 1 -> 
		    undefined 
	    end;
	{Port,{exit_status,ExitStatus}} ->
	    receive 
		{Port,eof} -> 
		    ok after 1 -> ok end,
	    ExitStatus;
	{Port,{data,{Tag,Data}}} ->
	    io:put_chars(Data),
	    case Tag of	eol -> io:nl(); noeol -> ok end,
	    job(Port);
	{Port,{data,Data}} ->
	    io:put_chars(Data),
	    job(Port);
	{'EXIT',Port,Reason} ->
	    {error,Reason};
	Other ->
	    io:format("WARNING: Unexpected at ~s:~p: ~p~n", 
		      [?MODULE_STRING,?LINE,Other]),
	    job(Port)
    end.



%% Thorough and safe altname.
%% Returns altname path (DOS name on Windows) or {error,Reason}
%%
altname(Path) when is_list(Path) ->
    try altname_1(filename:split(Path))
    catch 
	{error,enotsup} -> Path;
	Error -> Error
    end.

altname_1([]) -> [];
altname_1([F|Fs]) -> 
    altname_2(altname_x(F), Fs).

altname_2(F0, []) -> F0;
altname_2(F0, [F|Fs]) ->
    altname_2(filename:join(F0, altname_x(filename:join(F0, F))), Fs).

altname_x(Path) ->
    case file:altname(Path) of
	{ok,R} -> R;
	Error -> erlang:throw(Error)
    end.



%% Universal argument quoting
%%
%% If the string contains singlequote, doublequote or whitespace
%% - doublequote the string and singlequote embedded doublequotes.
%% God may forbid doublequotes. They do not work in Windows filenames, 
%% nor in YafRay result .tga filenames. They might only work in 
%% Unix executable pathname being very weird even there.
%%
quote(Cs) when is_list(Cs) ->
    case quote_needed(Cs) of
	true -> [$"|quote_1(Cs)];
	false -> Cs
    end.

quote_1([]) -> [$"];
quote_1([$"]) -> [$",$',$",$'];
quote_1([$"|Cs]) -> [$",$',$",$',$"|quote_1(Cs)];
quote_1([L|Cs]) when is_list(L) -> 
    [quote_1(L),$ |quote_1(Cs)];
quote_1([C|Cs]) -> [C|quote_1(Cs)].

quote_needed([]) -> false;
quote_needed([$"|_]) -> true;
quote_needed([$'|_]) -> true;
quote_needed([$\s|_]) -> true;
quote_needed([$\t|_]) -> true;
quote_needed([$\r|_]) -> true;
quote_needed([$\n|_]) -> true;
quote_needed([L|Cs]) when is_list(L) -> 
    quote_needed(L) orelse quote_needed(Cs);
quote_needed([_|Cs]) -> quote_needed(Cs).



%% Returns the now() time difference as a deep string in 
%% S.sss", M'S.sss" or H:M'S.sss" deep string format.
timestr({A1,B1,C1}, {A2,B2,C2}) ->
    timestr(((A1-A2)*1000000 + B1 - B2)*1000000 + C1 - C2).

%% Returns the argument time in microseconds in the
%% same format as timestr/2.
timestr(T) when is_integer(T), T < 0 -> 
    [$-|timestr_2(-T)];
timestr(T) when is_integer(T) ->
    timestr_2(T).

timestr_2(T0) ->
    Us = T0 rem 60000000,
    T1 = T0 div 60000000,
    M = T1 rem 60,
    H = T1 div 60,
    case {H,M} of
	{0,0} -> timestr_us(Us);
	{0,_} -> timestr_m(M, Us);
	{_,_} -> timestr_h(H, M, Us)
    end.

timestr_h(H, M, Us) ->
    [integer_to_list(H),$:|timestr_m(M, Us)].

timestr_m(M, Us) ->
    [integer_to_list(M),$'|timestr_us(Us)].

timestr_us(Us) ->
    io_lib:format("~.3f\"", [Us/1000000.0]).



%% Create a string unique for the OS process and time. It consists 
%% of the OS process id, a dash, and seconds since Jan 1 1970
%% encoded in approx 8 characters. It should be unique even in 
%% the event of an OS restart if called not more often than once
%% a second provided that an OS restart takes more than one second.
uniqstr() ->
    {Ms,S,_} = os:timestamp(),
    os:getpid()++"-"++uniqstr(Ms*1000000 + S).
%%
-define(UNIQBASE, (10+$Z-$A+1)).
uniqstr(0) -> [];
uniqstr(N) ->
    case N rem ?UNIQBASE of
	M when M < 10 ->
	    [$0+M|uniqstr(N div ?UNIQBASE)];
	M ->
	    [$A+M-10|uniqstr(N div ?UNIQBASE)]
    end.

