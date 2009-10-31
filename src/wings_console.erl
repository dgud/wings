%%
%%  wings_console.erl --
%%
%%     Console for Wings.
%%
%%  Copyright (c) 2004-2009 Raimo Niskanen
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_console).

%% I/O server and console server
-export([start/0,start/1,get_pid/0,stop/0,stop/1,
	 get_lines/0,get_all_lines/0,position/1,
	 setopts/1,getopts/1]).

%% Wings window
-export([window/0,window/1,window/4,popup_window/0]).

-define(SERVER_NAME, ?MODULE).
-define(WIN_NAME, console).
-define(DIRTY_TIME, 250).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").
-import(lists, [reverse/1,reverse/2,foldl/3]).
-import(erlang, [min/2,max/2]).

%% Debug exports
-export([code_change/0,get_state/0]).

%% Internal exports
-export([do_code_change/3]).

%%% I/O server state record ---------------------------------------------------

-record(state, {gmon,			% Monitor ref of original group leader
		group_leader,		% pid()
		width=80,		% Constant property
		height=24,		% -"-
		save_lines=100,		% -"-
		cnt=1,			% Queued lines incl last
		lines=queue:new(),	% Queue of binaries, head is oldest
		bef=[],			% Last line before cursor reversed
		aft=[],			% Last line after cursor
		pos=0,			% 0 is bottom, 1.. is start line
		%% The rate of dirty markings has to be low because of
		%% the main loop design in wings.erl. These fields are
		%% used for caching dirty events.
		tref,			% Timer ref
		dirty=false		% true | false
	       }).
-define(STATE, {state,Gmon,GroupLeader,Width,Height,SaveLines,
		Cnt,Lines,Bef,Aft,Pos,Tref,Dirty}).

%%% API -----------------------------------------------------------------------

start() ->
    start(group_leader()).

start(GroupLeader) when is_pid(GroupLeader) ->
    case whereis(?SERVER_NAME) of
	Server when is_pid(Server) ->
	    exit(already_started);
	undefined ->
	    Starter = self(),
	    Server = 
		spawn(
		  fun() ->
			  Self = self(),
			  case catch register(?SERVER_NAME, Self) of
			      true ->
				  Starter ! {wings_console_started,Self},
				  Gmon = erlang:monitor(process, GroupLeader),
				  server_loop(
				    #state{gmon=Gmon,
					   group_leader=GroupLeader});
			      _ ->
				  exit(already_started)
			  end
		  end),
	    Mref = erlang:monitor(process, Server),
	    receive
		{wings_console_started,Server} ->
		    demonitor(Mref),
		    Server;
		{'DOWN',Mref,_,_,Reason} -> 
		    exit(Reason)
	    end
    end.

get_pid() ->
    case whereis(?SERVER_NAME) of
	Server when is_pid(Server) ->
	    Server;
	undefined ->
	    exit(not_started)
    end.

stop() -> req({stop,normal}).
stop(Reason) -> req({stop,Reason}).

get_lines() -> 
    {Cnt,Pos,Lines} = req(get_lines),
    {Cnt,Pos,fix_lines(Lines)}.

get_all_lines() ->
    fix_lines(req(get_all_lines)).

position({absolute,_}=Pos) -> req({position,Pos});
position({relative,_}=Pos) -> req({position,Pos}).

setopts(Opts) when is_list(Opts) -> req({setopts,Opts}).

getopts(Opts) when is_list(Opts) -> req({getopts,Opts}).

window() ->
    window(?WIN_NAME).

window(Name) ->
    do_window(Name).

window(Name, Pos, Size, Ps) ->
    do_window(Name, Pos, Size, Ps).

popup_window() ->
    case wings_wm:is_window(?WIN_NAME) of
	true -> 
	    wings_wm:show(?WIN_NAME);
	false -> 
	    window()
    end.

%%%
%%% Debug API
%%%

get_state() -> req(get_state).

code_change() -> req(code_change).

%%%
%%% API helpers
%%%

fix_lines(Lines) ->
    [case L of
	 {Tag,B} when is_atom(Tag), is_binary(B) -> 
	     {Tag,binary_to_list(B)};
	 {Tag,B} when is_atom(Tag), is_list(B) -> 
	     L;
	 {Bef,Aft} when is_list(Bef), is_list(Aft) -> 
	     L 
     end || L <- Lines].

%%% End of API ----------------------------------------------------------------

%%%
%%% Scrollable console window.
%%%

-record(s, {width=24,height=80,
	    color,text_color,cursor_color}).

do_window(Name) ->
    Width0 = wings_pref:get_value(console_width),
    Height0 = wings_pref:get_value(console_height),
    wings_wm:delete(Name),
    {X1,_,W1,H1} = wings_wm:viewport(desktop),
    Font = wings_pref:get_value(new_console_font),
    CwLh = wings_io:use_font(Font, fun() -> {?CHAR_WIDTH,?LINE_HEIGHT} end),
    {Cw,Lh} = CwLh,
    Sw = wings_wm:vscroller_width(),
    Th = wings_wm:title_height(),
    %%
    W = min(3 + (Width0*Cw) + 3, W1-Sw),
    H = min(1 + (Height0*Lh) + 4, H1-Th),
    Size = {W,H},
    PosUR = {X1+W+Sw,H1-(H+Th)},
    do_window(Name, Font, CwLh, PosUR, Size, []).

do_window(Name, Pos, Size, Ps) ->
    Font = wings_pref:get_value(new_console_font),
    CwLh = wings_io:use_font(Font, fun() -> {?CHAR_WIDTH,?LINE_HEIGHT} end),
    do_window(Name, Font, CwLh, Pos, Size, Ps).

do_window(Name, Font, {Cw,Lh}, {X,Y}, {W,H}=Size, Ps) -> % {X,Y} is upper right
    Width = (-3+W-3) div Cw,
    Height = (-1+H-4) div Lh,
    setopts([{width,Width},{height,Height},
	     {save_lines,wings_pref:get_value(console_save_lines)}]),
    S = #s{width=Width,height=Height,
	   color=wings_pref:get_value(console_color),
	   text_color=wings_pref:get_value(console_text_color),
	   cursor_color=wings_pref:get_value(console_cursor_color)},
    Op = {seq,push,get_event(S)},
    Title = ?STR(do_window,1,"Console ")++integer_to_list(Width)++?STR(do_window,2,"x")++integer_to_list(Height),
    Props = [{font,Font}],
    wings_wm:toplevel(Name, Title, {X,Y,highest}, Size,
		      [closable,vscroller,{anchor,ne},
		       {properties,Props}|Ps],
		      Op),
    wings_wm:dirty().

get_event(S) ->
    {replace,fun(Ev) ->
		     handle_event(Ev, S)
	     end}.
handle_event(redraw, S) ->
    redraw(S);
handle_event(close, _) ->
    delete;
handle_event({?MODULE,updated}, _S) ->
    wings_wm:dirty();
handle_event({action,Action}, S) ->
    handle_action(Action, S);
handle_event(#keyboard{sym=?SDLK_HOME,state=?SDL_PRESSED}, S) ->
    handle_event({set_knob_pos,0.0}, S);
handle_event(#keyboard{sym=?SDLK_END,state=?SDL_PRESSED}, S) ->
    handle_event({set_knob_pos,1.0}, S);
handle_event(#keyboard{sym=?SDLK_PAGEUP,state=?SDL_PRESSED}, S) ->
    handle_event(scroll_page_up, S);
handle_event(#keyboard{sym=?SDLK_PAGEDOWN,state=?SDL_PRESSED}, S) ->
    handle_event(scroll_page_down, S);
%%
handle_event({set_knob_pos,P}, #s{height=Height}) ->
    {Cnt,Pos} = position({absolute,P}),
    update_scroller(Height, Cnt, Pos);
handle_event(scroll_page_up, #s{height=Height}=S) ->
    zoom_step(-max(Height-1, 1), S);
handle_event(scroll_page_down, #s{height=Height}=S) ->
    zoom_step(max(Height-1, 1), S);
handle_event(#mousebutton{}=Ev, S) ->
    case wings_menu:is_popup_event(Ev) of
	no -> handle_event_1(Ev, S);
	{yes,X,Y,_} -> popup_menu(X, Y, S)
    end;
handle_event(Ev, S) ->
    handle_event_1(Ev, S).

handle_event_1(#mousebutton{button=4,state=?SDL_RELEASED}, S) ->
    zoom_step(-10, S);
handle_event_1(#mousebutton{button=5,state=?SDL_RELEASED}, S) ->
    zoom_step(10, S);
handle_event_1(#keyboard{sym=?SDLK_UP,state=?SDL_PRESSED}, S) ->
    zoom_step(-1, S);
handle_event_1(#keyboard{sym=?SDLK_DOWN,state=?SDL_PRESSED}, S) ->
    zoom_step(1, S);
handle_event_1(_Ev, _) -> 
%%%     erlang:display({?MODULE,?LINE,_Ev}),
    keep.

handle_action({popup,write_file}, _S) ->
    Dir = wings_pref:get_value(current_directory),
    Ps = [{title,?STR(handle_action,1,"Write")},{directory,Dir},
	  {ext,".txt"},{ext_desc,?STR(handle_action,3,"Text File")}],
    Fun = fun(Name) ->
		  case write_file(Name) of
		      ok -> keep;
		      {error,Reason} ->
			  Msg = io_lib:format(?STR(handle_action,4,"Write error: ~w"), [Reason]),
			  wings_u:message(Msg),
			  keep
		  end
	  end,
    wings_plugin:call_ui({file,save_dialog,Ps,Fun}).

write_file(Name) ->
    case file:open(Name, [write]) of
	{ok,F} ->
	    case
		lists:foldl(
		  fun ({eol,L}, ok) -> file:write(F, [L|io_lib:nl()]);
		      ({noeol,L}, ok) -> file:write(F, L);
		      ({Bef,[]}, ok) -> file:write(F, Bef);
		      ({Bef,Aft}, ok) -> file:write(F, [Bef,Aft,$\r|Bef]);
		      (_,Error) -> Error
		  end, ok, get_all_lines())
		of
		ok -> file:close(F);
		Error ->
		    file:close(F),
		    Error
	    end;
	Error -> Error
    end.

popup_menu(X, Y, _S) ->
    Menu =
	[{?STR(popup_menu,1,"Write to File"),write_file,
	  ?STR(popup_menu,2,"Write console contents to a file")}],
    wings_menu:popup_menu(X, Y, popup, Menu).

zoom_step(Step, #s{height=Height}) ->
    {Cnt,Pos} = position({relative,Step}),
    update_scroller(Height, Cnt, Pos),
    keep.

redraw(#s{height=Height,
	  color=Color,text_color=TextColor,cursor_color=CursorColor}) ->
    {Cnt,Pos,Lines} = get_lines(),
    update_scroller(Height, Cnt, Pos),
    wings_io:ortho_setup(),
    {W,H} = wings_wm:win_size(),
    Lh = ?LINE_HEIGHT,
    W0 = -3+W-3,
    wings_io:border(0, 0, W-1, H-1, Color),
%%%    gl:translated(4, Lh, 0),
    foldl(fun 
	      ({Bef,Aft}, Y) when is_list(Bef), is_list(Aft) ->
		  gl:color3fv(TextColor),
		  wings_io:text_at(4, Y, Bef),
		  Bw = wings_text:width(Bef),
		  wings_io:text_at(4+Bw, Y, Aft),
		  gl:color3fv(CursorColor),
		  wings_io:text_at(4+Bw, Y, [caret]),
		  Lw = Bw + wings_text:width(Aft),
		  if Lw > W0 ->
			  wings_io:border(W-2, Y-Lh+3, 1, Lh, 
					  TextColor, TextColor);
		     true -> ok end,
		  Y+Lh;
	      ({Tag,L}, Y) when is_atom(Tag), is_list(L) ->
		  gl:color3fv(TextColor),
		  wings_io:text_at(4, Y, L),
		  Lw = wings_text:width(L),
		  if Lw > W0 ->
			  wings_io:border(W-2, Y-Lh+3, 1, Lh, 
					  TextColor, TextColor);
		     true -> ok end,
		  case Tag of
		      noeol ->
			  wings_io:border(0, Y+3, 1, Lh, 
					  TextColor, TextColor);
		      eol -> ok end,
		  Y+Lh
	  end, Lh, Lines),
    keep.

update_scroller(Height, Cnt, Pos) ->
    Name = wings_wm:this(),
    if Pos == 0, Cnt =< Height ->
	    wings_wm:set_knob(Name, 0.0, 1.0);
       Pos == 0 ->
	    wings_wm:set_knob(Name, (Cnt-Height)/Cnt, Height/Cnt);
       true ->
	    wings_wm:set_knob(Name, (Pos-1)/Cnt, Height/Cnt)
    end,
    wings_wm:dirty().

%%% I/O server ----------------------------------------------------------------

req(Request) ->
    case whereis(?SERVER_NAME) of
	Server when is_pid(Server) ->
	    req(Server, Request);
	_ ->
	    exit(not_started)
    end.

req(Server, Request) ->
    Mref = erlang:monitor(process, Server),
    Server ! {wings_console_request,self(),Mref,Request},
    receive
	{wings_console_reply,Mref,Reply} ->
	    demonitor(Mref),
	    Reply;
	{'DOWN',Mref,_,_,Reason} ->
	    exit(Reason)
    end.

demonitor(Mref) ->
    erlang:demonitor(Mref),
    receive {'DOWN',Mref,_,_,_} -> ok after 0 -> ok end.

server_loop(#state{gmon=Gmon,tref=Tref}=State) ->
    receive
	{io_request,From,ReplyAs,Request}=Msg when is_pid(From) ->
	    case io_request(State, Request) of
		{NewState,_,forward} ->
		    forward(Msg),
		    server_loop(NewState);
		{NewState,Reply,_} ->
		    io_reply(From, ReplyAs, Reply),
		    server_loop(NewState)
	    end;
	{wings_console_request,From,ReplyAs,{stop,Reason}} when is_pid(From) ->
	    wings_console_reply(From, ReplyAs, State#state.group_leader),
	    exit(Reason);
	{wings_console_request,From,ReplyAs,code_change} when is_pid(From) ->
%%% Code change exit point from old module
	    ?MODULE:do_code_change(State, From, ReplyAs);
	{wings_console_request,From,ReplyAs,Request} when is_pid(From) ->
	    {NewState,Reply} = wings_console_request(State, Request),
	    wings_console_reply(From, ReplyAs, Reply),
	    server_loop(NewState);
	{'DOWN',Gmon,_,_,Reason} ->
	    %% Group leader is down - die
	    exit(Reason);
	{timeout,Tref,dirty} ->
	    case State#state.dirty of
		true ->
		    NewTref = send_update(),
		    server_loop(State#state{tref=NewTref,dirty=false});
		false ->
		    server_loop(State#state{tref=undefined})
	    end;
	Unknown ->
	    io:format(?MODULE_STRING++?STR(server_loop,1,":~w Received unknown: ~p~n"), 
		      [?LINE,Unknown]),
	    server_loop(State)
    end.

send_update() ->
    catch wings_wm:psend(?WIN_NAME, {?MODULE,updated}),
    erlang:start_timer(?DIRTY_TIME, self(), dirty).


io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply,ReplyAs,Reply}.

wings_console_reply(From, ReplyAs, Reply) ->
    From ! {wings_console_reply,ReplyAs,Reply}.

forward({io_request,From,ReplyAs,Req0}) ->
    Req = forward_1(Req0),
    group_leader() ! {io_request,From,ReplyAs,Req}.
    
forward_1({put_chars,unicode,Chars}) when is_binary(Chars) ->
    {put_chars,Chars};
forward_1({put_chars,unicode,Chars}) when is_list(Chars) ->
    try
	{put_chars,list_to_binary(Chars)}
    catch
	error:badarg ->
	    {put_chars,filter_chars(Chars)}
    end;
forward_1({put_chars,Chars}) when is_list(Chars) ->
    try
	{put_chars,list_to_binary(Chars)}
    catch
	error:badarg ->
	    {put_chars,filter_chars(Chars)}
    end;
forward_1({put_chars,Chars}=Req) when is_binary(Chars) ->
    Req;
forward_1({put_chars,unicode,Mod,Func,Args}) ->
    forward_1({put_chars,unicode,apply(Mod, Func, Args)});
forward_1({put_chars,Mod,Func,Args}) ->
    forward_1({put_chars,apply(Mod, Func, Args)}).

filter_chars([H|T]) when is_list(H) ->
    [filter_chars(H)|filter_chars(T)];
filter_chars([H|T]) when is_integer(H), 255 < H ->
    [$?,filter_chars(T)];
filter_chars([H|T]) ->
    [H|filter_chars(T)];
filter_chars([]) -> [].

%%%
%%% I/O requests
%%%

io_request(State, {put_chars,Chars}) ->
    {put_chars(State, Chars),ok,forward};
io_request(State, {put_chars,unicode,Chars}) ->
    {put_chars(State, unicode:characters_to_list(Chars)),ok,forward};
io_request(State, {put_chars,unicode,Mod,Func,Args}) ->
    case catch apply(Mod, Func, Args) of
	Chars when is_list(Chars); is_binary(Chars) ->
	    io_request(State, {put_chars,unicode,Chars});
	_ ->
	    {State,{error,Func},error}
    end;
io_request(State, {put_chars,Mod,Func,Args}) ->
    case catch apply(Mod, Func, Args) of
	Chars when is_list(Chars); is_binary(Chars) ->
	    io_request(State, {put_chars,Chars});
	_ ->
	    {State,{error,Func},error}
    end;
io_request(State, {requests,Requests}) when is_list(Requests) ->
    io_request_loop(Requests, {State,ok,ok});
io_request(State, {setopts,Opts}) when is_list(Opts) ->
    {State,{error,badarg},error};
io_request(State, Request) ->
    %% Probably a new version of Erlang/OTP with extensions to the
    %% I/O protocol. We could generate an error here, but the
    %% stack dump in wings_crash.dump would generate the caller of
    %% io:format/2 in the main Wings process with no indication that
    %% this module is the culprit. Therefore, we choose to ignore
    %% the request, but write a message to the console to point out
    %% the problem.
    S = io_lib:format("Internal error in Console - unknown I/O request:\n~P\n",
		      [Request,10]),
    {put_chars(State, iolist_to_binary(S)),ok,forward}.

io_request_loop([], Result) ->
    Result;
io_request_loop([_|_], {_,_,error}=Result) ->
    Result;
io_request_loop([Request|Requests], {State,_,ok}) ->
    io_request_loop(Requests, io_request(State, Request)).

put_chars(State, Chars) when is_binary(Chars) ->
    put_chars_1(State, binary_to_list(Chars));
put_chars(State, Chars) when is_list(Chars) ->
    put_chars_1(State, Chars).

put_chars_1(#state{cnt=Cnt,lines=Lines,bef=Bef,aft=Aft}=State, Chars) 
  when is_list(Chars) ->
    put_chars_2(State, Chars, Cnt, Lines, Bef, Aft, length(Bef), []).

put_chars_2(#state{tref=undefined}=State, [], Cnt, Lines, Bef, Aft, _N, []) ->
    Tref = send_update(),
    State#state{cnt=Cnt,lines=Lines,bef=Bef,aft=Aft,tref=Tref,dirty=false};
put_chars_2(State, [], Cnt, Lines, Bef, Aft, _N, []) ->
    State#state{cnt=Cnt,lines=Lines,bef=Bef,aft=Aft,dirty=true};
put_chars_2(State, [], Cnt, Lines, Bef, Aft, N, [Chars|Stack]) ->
    put_chars_2(State, Chars, Cnt, Lines, Bef, Aft, N, Stack);
put_chars_2(State, [$\t|Chars], Cnt, Lines, Bef, Aft, N, Stack) ->
    Spaces = lists:duplicate(8 - (N rem 8), $\s),
    put_chars_2(State, Spaces++Chars, Cnt, Lines, Bef, Aft, N, Stack);
put_chars_2(State, [$\r|Chars], Cnt, Lines, Bef, Aft, _N, Stack) ->
    put_chars_2(State, Chars, Cnt, Lines, [], reverse(Bef, Aft), 0, Stack);
put_chars_2(State, [$\n|Chars], Cnt, Lines, Bef, Aft, _N, Stack) ->
    put_chars_nl(State, Chars, Cnt, Lines, Bef, Aft, 0, Stack, eol);
put_chars_2(State, [List|Chars], Cnt, Lines, Bef, Aft, N, Stack) 
  when is_list(List) ->
    put_chars_2(State, List, Cnt, Lines, Bef, Aft, N, [Chars|Stack]);
put_chars_2(#state{width=Width}=State, Chars, Cnt, Lines, Bef, [], N, Stack)
  when N > 0, (N rem Width) == 0 ->
    put_chars_nl(State, Chars, Cnt, Lines, Bef, [], N+1, Stack, noeol);
put_chars_2(State, [Char|Chars], Cnt, Lines, Bef, [], N, Stack) ->
    put_chars_2(State, Chars, Cnt, Lines, [Char|Bef], [], N+1, Stack);
put_chars_2(State, [Char|Chars], Cnt, Lines, Bef, [_|Aft], N, Stack) ->
    put_chars_2(State, Chars, Cnt, Lines, [Char|Bef], Aft, N+1, Stack).

put_chars_nl(#state{height=Height,save_lines=SaveLines,pos=Pos}=State0, 
	     Chars, Cnt0, Lines0, Bef, Aft, N, Stack, Tag) ->
    Line0 = reverse(Bef, Aft),
    Line = try
	       list_to_binary(Line0)
	   catch
	       error:badarg -> Line0
	   end,
    Lines1 = queue:snoc(Lines0, {Tag,Line}),
    Cnt = Cnt0 + 1,
    H = Height+SaveLines,
    if Cnt > H ->
	    State = 
		if Pos == 0 -> State0; 
		   true -> State0#state{pos=max(Pos-(Cnt-H), 1)} 
		end,
	    if Cnt == H+1 ->
		    %% This is just an optimization of the clause below
		    Lines = queue:tail(Lines1),
		    put_chars_2(State, Chars, H, Lines, [], [], N, Stack);
	       true ->
		    {_,Lines} = queue:split(Cnt0-H, Lines1),
		    put_chars_2(State, Chars, H, Lines, [], [], N, Stack)
	    end;
       true ->
	    put_chars_2(State0, Chars, Cnt, Lines1, [], [], N, Stack)
    end.

%%%
%%% Wings console requests
%%%

wings_console_request(State, get_lines) ->
    {State,wc_get_lines(State)};
wings_console_request(State, get_all_lines) ->
    {State,wc_get_all_lines(State)};
wings_console_request(#state{cnt=Cnt}=State, 
		      {position,{absolute,P}}) 
  when is_float(P), 0.0 =< P, P =< 1.0  ->
    wings_console_request(State, {position,{absolute,1+round(P*Cnt)}});
wings_console_request(#state{cnt=Cnt,pos=Pos0}=State, 
		      {position,{absolute,Pos1}}) 
  when is_integer(Pos1), Pos1 >= 1 ->
    case wc_position(State, Pos1) of
	Pos0 -> {State,{Cnt,Pos0}};
	Pos -> {State#state{pos=Pos},{Cnt,Pos}}
    end;
wings_console_request(#state{cnt=Cnt}=State, 
		      {position,{relative,P}}) 
  when is_float(P), -1.0 =< P, P =< 1.0  ->
    wings_console_request(State, {position,{relative,round(P*Cnt)}});
wings_console_request(#state{height=Height,cnt=Cnt,pos=Pos0}=State, 
		      {position,{relative,Pos1}})
  when is_integer(Pos1) ->
    Pos2 = if Pos0 == 0, Cnt =< Height -> 1;
	      Pos0 == 0 -> Cnt+1-Height;
	      true -> Pos0 
	   end + Pos1,
    case wc_position(State, max(Pos2, 1)) of
	Pos0 -> {State,{Cnt,Pos0}};
	Pos -> {State#state{pos=Pos},{Cnt,Pos}}
    end;
wings_console_request(State, {setopts,Opts}) ->
    wc_setopts(State, Opts);
wings_console_request(State, {getopts,Opts}) ->
    wc_getopts(State, Opts, []);
wings_console_request(State, get_state) ->
    {State,State};
wings_console_request(State, Request) ->
    {State,{error,{request,Request}}}.

wc_get_all_lines(#state{lines=Lines0,bef=Bef,aft=Aft}) ->
    Lines = queue:snoc(Lines0, {reverse(Bef),Aft}),
    queue:to_list(Lines).

wc_get_lines(#state{height=Height,cnt=Cnt,lines=Lines0,
		    bef=Bef,aft=Aft,pos=Pos}) ->
    Lines1 = queue:snoc(Lines0, {reverse(Bef),Aft}),
    Lines =
	if Pos =:= 0 ->
		M = max(Cnt-Height, 0),
		{_,Q} = queue:split(M, Lines1),
		Q;
	   Pos-1 >= Cnt ->
		{0,queue:new()};
	   Pos-1+Height >= Cnt ->
		{_,Q} = queue:split(Pos-1, Lines1),
		Q;
	   true ->
		{_,Q0} = queue:split(Pos-1, Lines1),
		{Q,_} = queue:split(Height, Q0),
		Q
	end,
    {Cnt,Pos,queue:to_list(Lines)}.

wc_position(#state{height=Height,cnt=Cnt}, Pos) ->
    if Pos-1+Height >= Cnt -> 0;
       true -> Pos
    end.

wc_setopts(#state{width=Width0,height=Height0,save_lines=SaveLines0}=State, 
	   Opts) ->
    Width = proplists:get_value(width, Opts, Width0),
    Height = proplists:get_value(height, Opts, Height0),
    SaveLines = proplists:get_value(save_lines, Opts, SaveLines0),
    if is_integer(Width), is_integer(Height), is_integer(SaveLines),
       Width >= 1, Height >= 1, SaveLines >= 0 ->
	    {State#state{width=Width,height=Height,save_lines=SaveLines},ok};
       true ->
	    {State,{error,badarg}}
    end.

wc_getopts(State, [], R) ->
    {State,reverse(R)};
wc_getopts(#state{width=Width}=State, [width|Opts], R) ->
    wc_getopts(State, Opts, [{width,Width}|R]);
wc_getopts(#state{height=Height}=State, [height|Opts], R) ->
    wc_getopts(State, Opts, [{height,Height}|R]);
wc_getopts(#state{save_lines=SaveLines}=State, [save_lines|Opts], R) ->
    wc_getopts(State, Opts, [{save_lines,SaveLines}|R]);
wc_getopts(State, _, _) ->
    {State,{error,badarg}}.

%%% Other support functions
%%%

%% Code change entry point in the new module. Called in the new module
%% with the state from the old module.
%%
%% Intended for development.
%%
do_code_change(?STATE, From, ReplyAs) ->
    wings_console_reply(From, ReplyAs, ok),
    server_loop(#state{gmon=Gmon,group_leader=GroupLeader,
		       width=Width,height=Height,save_lines=SaveLines,
		       cnt=Cnt,lines=Lines,bef=Bef,aft=Aft,
		       pos=Pos,tref=Tref,dirty=Dirty}).
