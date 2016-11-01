%%
%%  wpc_tt.erl --
%%
%%     Functions for reading TrueType fonts (.tt)
%%
%%  Copyright (c) 2001-2011 Howard Trickey
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_tt).
-export([init/0,menu/2,command/2,trygen/3,	% trygen is for debugging
	 findpolyareas/1,polyareas_to_faces/1,subdivide_pas/2]). % for ai

-import(lists, [reverse/1,sort/2,keysearch/3,duplicate/2,nthtail/2,
		mapfoldl/3,foldl/3,sublist/3,map/2,last/1,seq/2,seq/3,
		flatten/1,sum/1,append/1]).

-include_lib("wings/src/wings.hrl").
-include_lib("wings/e3d/e3d.hrl").

-record(ttfont,
	{nglyph,			% number of glyphs
	 uperem,			% units per em
	 cmap,				% 256-element tuple, maps char -> glyph
	 loca,				% (nglyph+1)-element tuple, maps glyph -> offset in glyf
	 adv,				% nglyph-element tuple: maps glyph -> amount to advance x
	 glyf}).			% glyf table (binary)

-record(polyarea,
	{boundary,			%list of cedges (CCW oriented, closed)
	 islands=[]}).			%list of lists of cedges (CW, closed)

						% a "possibly curved" edge, with explicit coords
						% and optional cubic bezier control points
-record(cedge,
	{vs,cp1=nil,cp2=nil,ve}).	%all are {x,y} pairs

-define (fsITALIC, 2#00000001).
-define (fsBOLD,   2#00100000).

-define(S16, 16/signed).
-define(U16, 16/unsigned).
-define(S32, 32/signed).
-define(U32, 32/unsigned).
-define(SKIP, _/binary).

init() -> true.

menu({shape}, Menu) ->
    insert_before(Menu);
menu(_, Menu) -> Menu.

insert_before([{_,grid,_,_}=Grid|Rest]) ->
    [Grid,separator,menu_entry()|Rest];
insert_before([H|T]) ->
    [H|insert_before(T)];
insert_before([]) ->
    [menu_entry()].

menu_entry() ->
    {?__(1,"Text"),text,?__(2,"Convert text to a 3D object"),[option]}.

command({shape,{text,Ask}}, St) -> make_text(Ask, St);
command(_, _) -> next.

make_text(Ask, St) when is_atom(Ask) ->
    FontDir  = sysfontdir(),
    DefFont  = default_font(),
    FontInfo = case wpa:pref_get(wpc_tt, fontname, DefFont) of
		   FI = #{type := font} -> FI;
		   _ -> DefFont
	       end,

    Text = wpa:pref_get(wpc_tt, text, "Wings 3D"),
    Bisect = wpa:pref_get(wpc_tt, bisections, 0),
    GbtFonts = process_ttfs(FontDir),
    %% io:format("FontList: ~p\n\n",[gb_trees:to_list(GbtFonts)]),
    Dlg =
    	[{vframe, [
	    {hframe,[
	    	{label, ?__(2,"Text")},
		{text,Text,[{key,{wpc_tt,text}}]},
		help_button()
		]},
	    {label_column, [
		{?__(5,"Number of edge bisections"),{slider,{text,Bisect,[{key,{wpc_tt,bisections}},{range,{0,3}}]}}},
		{?__(3,"TrueType font"), {fontpicker,FontInfo,[{key,{wpc_tt,font}}]}}]},
	    wings_shapes:transform_obj_dlg()
	],[{margin,false}]
	 }],
    wings_dialog:dialog(Ask,?__(1,"Create Text"), {preview, Dlg},
			fun({dialog_preview,[T,N,{_,Ctrl},RX,RY,RZ,MX,MY,MZ,Grnd]=_Res}) ->
				{_, FPath} = get_font_file(GbtFonts,Ctrl),
				{preview,{shape,{text,[T,N,{fontdir,FPath},RX,RY,RZ,MX,MY,MZ,Grnd]}},St};
			   (cancel) ->
				St;
			   ([T,N,{_,WxFont},RX,RY,RZ,MX,MY,MZ,Grnd]=_Res) when is_tuple(WxFont) ->
				{NewFontI, FPath} = get_font_file(GbtFonts,WxFont),
				wpa:pref_set(wpc_tt, fontname, NewFontI),
				wpa:pref_set(wpc_tt, text, element(2,T)),
				wpa:pref_set(wpc_tt, bisections, element(2,N)),
				{commit,{shape,{text,[T,N,{fontdir,FPath},RX,RY,RZ,MX,MY,MZ,Grnd]}},St}
			end);

make_text([{_,T},{_,N},{_,DirFont}|Transf], _) ->
    F = filename:basename(DirFont),
    D = filename:dirname(DirFont),
    gen(F, D, T, N, Transf).

help_button() ->
    Title = ?__(1,"Browsing for Fonts on Windows"),
    TextFun = fun () -> help() end,
    {help,Title,TextFun}.

help() ->
    [?__(1,"Only TrueType fonts can be used and they must be"
	 "installed in standard operating system directory for fonts.")].

gen(_Font, _Dir, "", _Nsubsteps, _Transf) ->
    keep;
gen(Font, Dir, Text, Nsubsteps, Transf) ->
    File = font_file(Font, Dir),
    case catch trygen(File, Text, Nsubsteps) of
	{new_shape,Name,Fs0,Vs0,He} ->
	    Vs = wings_shapes:transform_obj(Transf, Vs0),
	    Fs = [#e3d_face{vs=Vsidx} || Vsidx <- Fs0],
	    Mesh = #e3d_mesh{type=polygon, vs=Vs, fs=Fs, he=He},
	    {new_shape, Name, #e3d_object{obj=Mesh}, []};
	{error,"no such file or directory"} ->
	    wpa:error_msg(?__(4,"Text failed: failed to locate the TTF file for the selected font"));
	{error,Reason} ->
	    wpa:error_msg(?__(1,"Text failed: ") ++ Reason);
	X ->
	    io:format(?__(2,"caught error: ") ++"~p~n", [X]),
	    wpa:error_msg(?__(3,"Text failed: internal error"))
    end.

process_ttfs(Dir) ->
    Add = fun(FileName, Acc) ->
		  case read_ttf_name(FileName) of
		      {FName,FStyle,FWeight} ->
			  gb_trees:enter({FName,FStyle,FWeight},FileName,Acc);
		      _ -> Acc
		  end
	  end,
    filelib:fold_files(Dir, ".ttf|.TTF", true, Add, gb_trees:empty()).

read_ttf_name(File) ->
    case file:read_file(File) of
	{ok,Filecontents} ->
	    try
		{ok, TTFpart} = ttfpart(Filecontents),
		(TTFpart =:= Filecontents) orelse erlang:display({changed, File}),
		parsett_header(File, Filecontents)
	    catch _ET:_Error ->
		    io:format("ERROR: ~s:~P ~p~n",[File,_Error, 10, erlang:get_stacktrace()]),
		    error
	    end;
	{error,_Reason} ->
	    error
    end.

%% The Font dialog can show options for styles which we are not able to find a file like the "Italic"
%% and "Italic Bold" for "Comic Sans MS" - there are no files for these styles (only Regular and
%% Bold). Here, we'll try to get the font with a most close appearance of that selected by the user.
get_font_file(GbtFonts, WxFont) ->
    FontInfo = wings_text:get_font_info(WxFont),
    #{face:=FName, style:=FStyle, weight:=FWeight} = FontInfo,
    case get_font_file(0,GbtFonts,FName,FStyle,FWeight) of
        undefined -> {FontInfo, "unknown"};
        FPath -> {FontInfo, FPath}
    end.

get_font_file(0=Try,GbtFonts,FName,FStyle,FWeight) ->  % try to get the right fount
    case gb_trees:lookup({FName,FStyle,FWeight},GbtFonts) of
	{value,FPath} -> FPath;
        _ -> get_font_file(Try+1,GbtFonts,FName,normal,FWeight)
    end;
get_font_file(1=Try,GbtFonts,FName,FStyle,FWeight) ->  % try to get the right fount
    case gb_trees:lookup({FName,FStyle,FWeight},GbtFonts) of
	{value,FPath} -> FPath;
        _ -> get_font_file(Try+1,GbtFonts,FName,FStyle,normal)
    end;
get_font_file(2,GbtFonts,FName,FStyle,FWeight) ->  % try to get the right fount
    case gb_trees:lookup({FName,FStyle,FWeight},GbtFonts) of
        {value,FPath} -> FPath;
        _ -> win_font_substitutes(FName,GbtFonts)
    end.

parsett_header(_File, Bin) ->
    FontInfo = font_info(Bin),
    Family = proplists:get_value(family, FontInfo, undefined),
    {Style, Weight} = font_styles(Bin),
    %% io:format("File: ~p ~p ~p ~p~n", [_File,Family, Style, Weight]),
    {Family,Style,Weight}.

%% Return the requested string from font
%% By default font family and subfamily (if not regular)
font_info(Font) ->
    StdInfoItems = [info(1),info(2),info(3),info(4),info(16),info(17)],
    Try = [{StdInfoItems, microsoft, unicode, english},
	   {StdInfoItems, unicode, unicode, 0},
	   {StdInfoItems, mac, roman, english}
	  ],
    font_info_2(Font, Try).

font_info_2(Font, [{Id,Platform,Enc,Lang}|Rest]) ->
    case font_info(Font, Id, Platform, Enc, Lang) of
	[] -> font_info_2(Font, Rest);
	Info -> Info
    end;
font_info_2(_, []) -> [].


%% Return the requested string from font
%% Info Items: 1,2,3,4,16,17 may be interesting
%% Returns a list if the encoding is known otherwise a binary.
%% Return the empty list is no info that could be matched is found.
font_info(Bin, Id, Platform, Encoding, Language) ->
    case find_table(Bin, <<"name">>) of
	false -> [];
	Name ->
	    <<_:Name/binary, _:16, Count:?U16, StringOffset:?U16, FI/binary>> = Bin,
	    <<_:Name/binary, _:StringOffset/binary, Strings/binary>> = Bin,
	    get_font_info(Count, FI, Strings, Id, Platform, Encoding, Language)
    end.

get_font_info(0, _, _, _, _, _, _) -> [];
get_font_info(N, <<PId:?U16, EId:?U16, LId:?U16, NId:?U16,
		   Length:?U16, StrOffset:?U16, Rest/binary>>, Strings,
	      WIds, WPlatform, WEnc, WLang) ->
    <<_:StrOffset/binary, String:Length/binary, ?SKIP>> = Strings,
    Platform = platform(PId),
    Encoding = encoding(EId, Platform),
    Lang = language(LId, Platform),
    Enc = check_enc(Encoding, WEnc),
    case lists:member(info(NId), WIds) of
	true when Platform =:= WPlatform, Enc, Lang =:= WLang ->
	    %% io:format("Encoding ~p Platform ~p Eid ~p~n",[Encoding, Platform, EId]),
	    [{info(NId), string(String, Encoding)}|
	     get_font_info(N-1, Rest, Strings, WIds, WPlatform, WEnc, WLang)];
	_ ->
	    get_font_info(N-1, Rest, Strings, WIds, WPlatform, WEnc, WLang)
    end.

check_enc(A, A) -> true;
check_enc({unicode,_}, unicode) -> true;
check_enc({unicode,_,_}, unicode) -> true;
check_enc(_, _) -> false.

find_table(<<_:32, NumTables:?U16, _SR:16, _ES:16, _RS:16, Tables/binary>>, Tag) ->
    find_table(NumTables, Tag, Tables).

find_table(0, _, _) -> false;
find_table(_, Tag, <<Tag:4/binary, _CheckSum:32, Offset:?U32, _Len:32, ?SKIP>>) ->
    Offset;
find_table(Num, Tag, <<_Tag:4/binary, _CheckSum:32, _Offset:32, _Len:32, Next/binary>>) ->
    find_table(Num-1, Tag, Next).

platform(0) -> unicode;
platform(1) -> mac;
platform(2) -> iso;
platform(3) -> microsoft;
platform(Id) -> Id.

encoding(0, unicode) -> {unicode, {1,0}};
encoding(1, unicode) -> {unicode, {1,1}};
encoding(2, unicode) -> iso_10646;
encoding(3, unicode) -> {unicode, bmp, {2,0}};
encoding(4, unicode) -> {unicode, full,{2,0}};

encoding(0, microsoft)  -> symbol;
encoding(1, microsoft)  -> {unicode, bmp};
encoding(2, microsoft)  -> shiftjis;
encoding(10, microsoft) -> {unicode, bmp};

encoding(0, mac) ->  roman        ;
encoding(1, mac) ->  japanese     ;
encoding(2, mac) ->  chinese_trad ;
encoding(3, mac) ->  korean       ;
encoding(4, mac) ->  arabic       ;
encoding(5, mac) ->  hebrew       ;
encoding(6, mac) ->  greek        ;
encoding(7, mac) ->  russian      ;

encoding(Id, _) -> Id.

language(16#0409, microsoft) -> english ;
language(16#0804, microsoft) -> chinese ;
language(16#0413, microsoft) -> dutch   ;
language(16#040c, microsoft) -> french  ;
language(16#0407, microsoft) -> german  ;
language(16#040d, microsoft) -> hebrew  ;
language(16#0410, microsoft) -> italian ;
language(16#0411, microsoft) -> japanese;
language(16#0412, microsoft) -> korean  ;
language(16#0419, microsoft) -> russian ;
%%language(16#0409, microsoft) -> spanish ;
language(16#041d, microsoft) -> swedish ;

language(0 , mac) -> english ;
language(12, mac) -> arabic  ;
language(4 , mac) -> dutch   ;
language(1 , mac) -> french  ;
language(2 , mac) -> german  ;
language(10, mac) -> hebrew  ;
language(3 , mac) -> italian ;
language(11, mac) -> japanese;
language(23, mac) -> korean  ;
language(32, mac) -> russian ;
language(6 , mac) -> spanish ;
language(5 , mac) -> swedish ;
language(33, mac) -> chinese_simplified ;
language(19, mac) -> chinese ;

language(Id, _) -> Id.

info(0) -> copyright;
info(1) -> family;
info(2) -> subfamily;
info(3) -> unique_subfamily;
info(4) -> fullname;
info(5) -> version;
info(6) -> postscript_name;
info(7) -> trademark_notice;
info(8) -> manufacturer_name;
info(9) -> designer;
info(10) -> description;
info(11) -> url_vendor;
info(12) -> url_designer;
info(13) -> license_descr;
info(14) -> url_license;
%%info(15) -> reserved;
info(16) -> preferred_family;
info(17) -> preferred_subfamily;
%%info(18) -> compatible_full; %% Mac only
info(19) -> sample_text;
info(Id) -> Id.

string(String, roman) ->
    unicode:characters_to_list(String, latin1);
string(String, {unicode, _}) ->
    unicode:characters_to_list(String, utf16);
string(String, {unicode, bmp, _}) ->
    unicode:characters_to_list(String, utf16);
string(String, {unicode, full, _}) ->
    unicode:characters_to_list(String, utf32);
string(String, _) ->
    String.

font_styles(Bin) ->
    TabOffset = find_table(Bin, <<"OS/2">>),
    case is_integer(TabOffset) of
	true ->
	    <<_:TabOffset/binary,_Ver:16,_Pad:30/binary,_Panose:10/binary,_ChrRng:16/binary,
	      _VenId:4/binary,FsSel:16,_T1/binary>> = Bin,

	    FStyle = if (FsSel band ?fsITALIC) =:= ?fsITALIC -> italic;
			true -> normal
		     end,
	    FWeight = if (FsSel band ?fsBOLD) =:= ?fsBOLD -> bold;
			 true -> normal
		      end,
	    {FStyle, FWeight};
	false ->
	    {normal, normal}
    end.

trygen(File, Text, Nsubsteps) ->
    case file:read_file(File) of
	{ok,Filecontents} ->
	    case ttfpart(Filecontents) of
		{ok, TTFpart} ->
		    Ttf = parsett(TTFpart),
		    Pa = getpolyareas(Text, Ttf, Nsubsteps),
		    {Vs0,Fs,He} = polyareas_to_faces(Pa),
		    {CX,CY,CZ} = e3d_vec:average(tuple_to_list(e3d_bv:box(Vs0))),
		    Vs = [{X-CX,Y-CY,Z-CZ} || {X,Y,Z} <- Vs0],
		    {new_shape,"text",Fs,Vs,He}; % Would be nicer centered by centroid
		_ -> {error, ?__(1,"Can't find TrueType section in ") ++ File}
	    end;
	{error,Reason} ->
	    {error,file:format_error(Reason)}
    end.

%% Try to map a Name to a font file using registry
%% and in any case, concatenate Dir in front of it (if Dir != "").
%% If dir is empty and we didn't find it in the current dir,
%% try the sysfontdir again.
font_file(Name, Dir) ->
    Name1 = case Dir of
		"." -> Name;
		_ -> filename:join([Dir,Name])
	    end,
    case filelib:is_regular(Name1) of
	true -> Name1;
	_ ->
	    case os:type() of
		{win32,_} ->
		    Name2 = case winregval(?__(1,"Fonts"), Name ++ " (TrueType)") of
				none -> Name;
				Fname -> Fname
			    end,
		    case Dir of
			"." -> filename:join([sysfontdir(),Name2]);
			_ -> filename:absname(Dir ++ "\\" ++ Name2)
		    end;
		_ ->
		    case Dir of
			"." -> filename:join([sysfontdir(),Name]);
			_ -> Name1
		    end
	    end
    end.

%% Look up value with Name in Windows registry,
%% first changing to key K under the "CurrentVersion" for Windows.
%% Return value as string, or the token "none" if any problems.
winregval(K, Name) ->
    case os:type() of
	{win32,Wintype} ->
	    case win32reg:open([read]) of
		{ok, RH} ->
		    W = case Wintype of nt -> "Windows NT" ; _ -> "Windows" end,
		    CVK = "\\hklm\\SOFTWARE\\Microsoft\\" ++ W
			++ "\\CurrentVersion",
		    K1 = case K of
			     "" -> CVK;
			     _ -> CVK ++ "\\" ++ K
			 end,
		    Val = case win32reg:change_key(RH, K1) of
			      ok ->
				  case win32reg:value(RH, Name) of
				      {ok, V} -> V;
				      _ -> none
				  end;
			      _ -> none
			  end,
		    win32reg:close(RH),
		    Val;
		_ -> none
	    end;
	_ ->
	    none
    end.

win_font_substitutes(FName,GbtFonts) ->
    case os:type() of
	{win32,_} ->
	    case winregval(?__(1,"FontSubstitutes"),FName) of
		none -> undefined;
		FSName ->
		    case gb_trees:lookup({FSName,normal,normal},GbtFonts) of
			{value, FPath} -> FPath;
			_ -> undefined
		    end
	    end;
	_ -> undefined
    end.

%% Try to find default system directory for fonts
sysfontdir() ->
    case os:type() of
	{win32,Wintype} ->
	    SR = case winregval("", "SystemRoot") of
		     none ->
			 case Wintype of
			     nt -> "C:/winnt";
			     _ -> "C:/windows"
			 end;
		     Val -> Val
		 end,
	    SR ++ "/Fonts";
	{unix,Utype} ->
	    Dir = case Utype of
		      darwin -> "/Library/Fonts";
		      _ -> "/usr/share/fonts/"
		  end,
	    case file:list_dir(Dir) of
		{error, _} ->
		    "/~";
		_ ->
		    Dir
	    end
    end.

default_font() ->
    wings_text:get_font_info(wxSystemSettings:getFont(?wxSYS_DEFAULT_GUI_FONT)).


%% Return {Vs,Fs} corresponding to list of polyareas,
%% where Vs is list of coords and Fs is list of list of
%% coord indices, describing faces.
polyareas_to_faces(Pas) ->
    VFpairs = map(fun pa2object/1, Pas),
    concatvfs(VFpairs).

concatvfs(Vfp) -> concatvfs(Vfp, 0, [], [], []).

concatvfs([{Vs,Fs,HardEdges}|Rest], Offset, Vsacc, Fsacc, Hdacc) ->
    Fs1 = offsetfaces(Fs, Offset),
    He1 = offsetfaces(HardEdges, Offset),
    Off1 = Offset + length(Vs),
    concatvfs(Rest, Off1, [Vs|Vsacc], Fsacc ++ Fs1, He1 ++ Hdacc);
concatvfs([], _Offset, Vsacc, Fsacc, Hdacc) ->
    He = build_hard_edges(Hdacc, []),
    {flatten(reverse(Vsacc)),Fsacc, He}.

build_hard_edges([[First|_]=Loop|Rest], All) ->
    New = build_hard_edges(Loop, First, All),
    build_hard_edges(Rest, New);
build_hard_edges([], All) -> All.

build_hard_edges([A|[B|_]=Rest], First, All) ->
    build_hard_edges(Rest, First, [{A,B}|All]);
build_hard_edges([Last], First, All) ->
    [{Last, First}|All].


%% For TrueType format, see
%% http://developer.apple.com/fonts/TTRefMan/index.html
%% or
%% http://www.microsoft.com/typography/otspec/

ttfpart(Filecontents) ->
    case is_ttf(Filecontents, false) of
	true ->
	    {ok,Filecontents};
	_ ->
	    io:format("Not a ttf file", []),
	    case parse_dfont(Filecontents) of
		<<>> ->
		    case parse_embedded_ttf(Filecontents) of
			<<>> -> {error,<<>>};
			B -> {ok, B}
		    end;
		B -> {ok, B}
	    end
    end.

%% ttf fonts start with an "offset subtable":
%%  uint32 - tag to mark as TTF (one of the 0,1,0,0; "true"; or "OTTO")
%%  uint16 - number of directory tables
%%  uint16 - search range: (maximum power of 2 <= numTables)*16
%%  uint16 - entry selector: log2(maximum power of 2 <= numTables)
%%  uint16 - range shift: numTables*16-searchRange

is_ttf(<<0,1,0,0,R/binary>>, V) -> not V orelse is_ttf_fin(R);
is_ttf(<<"true",R/binary>>, V)  -> not V orelse is_ttf_fin(R);
%is_ttf(<<"typ1",R/binary>>, V)  -> not V orelse is_ttf_fin(R);
is_ttf(<<"OTTO",R/binary>>, V)  -> not V orelse is_ttf_fin(R);
is_ttf(<<1,0,0,0,R/binary>>, V) -> not V orelse is_ttf_fin(R);
is_ttf(_,_) -> false.

is_ttf_fin(<<NumTabs:?U16, SrchRng:?U16, _EntSel:?U16, Rsh:?U16, B/binary>>) ->
    NumTabs > 0 andalso (size(B) > NumTabs*16) andalso (Rsh == NumTabs*16 - SrchRng).

%% An Apple "dfont" has many resources
%% in a merged resource fork/data fork.
%% Some of those resources may be 'sfnt's, and those may by ttf format.
%% We could parse the resource map, find all the 'sfnt's, parse the
%% name/map at the end, and give the user a choice.
%% For now, just do the easy thing: source through all the resources
%% until find one that starts like a ttf font.
parse_dfont(<<Rpos:32, Mpos:32, Rlen:32, _Mlen:32, B/binary>>) ->
    Skip = Rpos - 16,
    case (Mpos == Rpos + Rlen) andalso (Rlen > 0)
	andalso (Skip >= 0) andalso (Skip < size(B)) of
	true ->
	    <<_Skipped:Skip/binary, B2/binary>> = B,
	    findttfres(B2, Rlen);
	_ -> <<>>
    end;
parse_dfont(_) -> <<>>.

findttfres(_, 0) -> <<>>;
findttfres(<<Reslen:32, B/binary>>, Rlenleft) ->
    if
	(size(B) >= Reslen) ->
	    <<Res:Reslen/binary, B2/binary>> = B,
	    case is_ttf(Res, true) of
		true -> Res;
		_ -> findttfres(B2, Rlenleft - Reslen - 4)
	    end;
	true -> <<>>
    end;
findttfres(_, _) -> {nil, << 0 >> }.

%% This is a desperation move to handle files whose format
%% we don't know, but might have a valid ttf section inside it
%% (some old Mac files are like this).
%% Just go byte-by-byte, looking for the start of a valid ttf section.
parse_embedded_ttf(B) ->
    case is_ttf(B, true) of
	true -> B;
	_ ->
	    case B of
		<<_C,Brest/binary>> -> parse_embedded_ttf(Brest);
		_ -> <<>>
	    end
    end.

%% Parse binary arg, which should be a TrueType file,
%% and return a ttfont.
%% Throws {error,reason} or a badmatch if the file format is wrong.
						%
%% After the offset table (see above), comes a number of table
%% directories (each with a 4-character tag), and then the binary
%% data making up the tables themselves (the directories have pointers
%% into the binary data).
%% We parse all the directories (Dirs), then use that to get all the
%% tables as binaries (Tabs), and then finally parse the tables we need.
%% The actual polygons are in the glyf table, which is parsed for
%% only the needed glyfs, later.
parsett(<<_C1,_C2,_C3,_C4,Ntabs:16/unsigned,_Srchrng:16/unsigned,
	  _Esel:16/unsigned,_Rngshift:16/unsigned,B/binary>>) ->
    {Dirs,B1} = getdirs(Ntabs,B),
    Dirs1 = sort(fun({X,_,_},{Y,_,_}) -> X < Y end, Dirs),
    Offset = 12 + (Ntabs*16),
    Tabs = gettabs(Dirs1, B1, Offset),
    Nglyph = parsemaxptab(Tabs),
    Cmap = parsecmaptab(Tabs),
    {Uperem, ShortLoca} = parseheadtab(Tabs),
    Loca = parselocatab(Tabs, Nglyph, ShortLoca),
    Nhmetrics = parsehheatab(Tabs),
    Adv = parsehmtxtab(Tabs, Nglyph, Nhmetrics),
    Glyf = findtab("glyf", Tabs),
    #ttfont{nglyph=Nglyph, uperem=Uperem, cmap=Cmap, loca=Loca, adv=Adv, glyf=Glyf};
parsett(_) ->
    throw({error,?__(1,"Bad offset table")}).

%% returns list of table directory entries: {offset,length,name} tuples
getdirs(Ntabs,B) -> getdirs(Ntabs,B,[]).

%% Table directory format:
%%  uint32 - tag (4 ascii chars identifying table kind)
%%  uint32 - checksum for this table
%%  uint32 - offset from beginning of ttf font
%%  uint32 - length of table in bytes (actual, not padded)
getdirs(0, B, Acc) ->
    {reverse(Acc),B};
getdirs(Nleft,<<W,X,Y,Z,_Csum:32,Off:32,Len:32,B/binary>>,Acc) ->
    getdirs(Nleft-1, B, [{Off,Len,[W,X,Y,Z]} | Acc]);
getdirs(_,_,_) ->
    throw({error,?__(1,"Bad dir format")}).

%% returns list of {tablename,table/binary} tuples
gettabs(Dirs,B,Offset) -> gettabs(Dirs,B,Offset,[]).

gettabs([],_,_,Acc) ->
    reverse(Acc);
gettabs([{Offnext,Len,Nam}|T]=Dirs,B,Off,Acc) ->
    if
	Off == Offnext ->
	    <<Tab:Len/binary,B1/binary>> = B,
	    gettabs(T, B1, Off+Len, [{Nam,Tab} | Acc]);
	Off < Offnext ->
	    Padlen = Offnext - Off,
	    <<_C:Padlen/binary,B1/binary>> = B,
	    gettabs(Dirs,B1,Offnext,Acc);
	true ->
	    throw({error,?__(1,"Bad table offsets/sizes")})
    end.

%% Find the table with the given name in Tabs and return it.
%% Throw error if not found.
findtab(Name, Tabs) ->
    case keysearch(Name, 1, Tabs) of
	{value, {_, Tab}} ->
	    Tab;
	false ->
	    throw({error,?__(1,"No ") ++ Name ++ ?__(2," table")})
    end.


%% Parse the "maxp" (Maximum Profile) tab of Tabs and return numGlyphs
parsemaxptab(Tabs) ->
    Tab = findtab("maxp", Tabs),
    <<16#00010000:32,NumGlyphs:16/unsigned,_/binary>> = Tab,
    NumGlyphs.

%% Parse the "cmap" (Character to Glyph Index) tab of Tabs.
%% Return 256-long tuple where element (c+1) is glyph number for character c.
%%
%% cmap table format:
%%  uint16 - version (should be 0)
%%  uint16 - number of subtables
%% followed by the subtables, each in format:
%%  uint16 - platformID
%%  uint16 - platform-specific encoding id
%%  uint32 - offset of mapping table
%%
%% Currently, we can handle format 0 (single byte table)
%% and format 4 (two-byte, segmented encoding format)
parsecmaptab(Tabs) ->
    Tab = findtab("cmap", Tabs),
    <<0:16,Nsubtabs:16,T1/binary>> = Tab,
    ST = getcmapsubtabs(Nsubtabs, T1, Tab, []),
    SortST = sort(fun cmapcmp/2, ST),
    case SortST of
	[{_P,_E,0,Off}|_] ->
	    list_to_tuple(binary_to_list(Tab,Off+1+6,Off+256+6));
	[{_P,_E,4,Off}|_] ->
	    cmapf4(Tab, Off);
	_ -> throw({error,?__(1,"No suitable character map")})
    end.

getcmapsubtabs(0, _, _, Acc) ->
    Acc;
getcmapsubtabs(N, <<Pid:16,Eid:16,Off:32,T/binary>>, Tab, Acc) ->
    {Fhigh,Flow} = list_to_tuple(binary_to_list(Tab,Off+1,Off+2)),
    Format = toushort(Fhigh,Flow),
    getcmapsubtabs(N-1, T, Tab, [{Pid,Eid,Format,Off}|Acc]).

%% Need a format 0 or 4 table,
%% prefer Platform 0 (Unicode),
%% and prefer Platform specific encodoing 1 in both cases
cmapcmp({P1,E1,F1,_},{P2,E2,F2,_}) ->
    if
	F1 == 0, F2 /= 0 -> true;
	F2 == 0 -> false;
	F1 == 4, F2 /= 4 -> true;
	F2 == 4 -> false;
	true ->
	    if
		P1 < P2 -> true;
		P1 > P2 -> false;
		true ->
		    if
			E1 == 1 -> true;
			E2 == 1 -> false;
			true -> E1 < E2
		    end
	    end
    end.

%% Format 4 cmap subtables have this format:
%%  uint16 - format (will be 4)
%%  uint16 - length in bytes of subtable
%%  uint16 - language
%%  uint16 - segcountX2 : 2 * segment count
%%  uint16 - searchRange : 2 * (2**Floor(log2 segcount))
%%  uint16 - entrySelector : log2(searchRange/2)
%%  uint16 - rangeShift : (2 * segCount) - searchRange
%%  uint16 * segCount : endCode[]: ending character code for each seg, FFFF last
%%  uint16 - reserved pad
%%  uint16 * segCount : startCode[]: starting character code for each seg
%%  uint16 * segCount : idDelta[]: delta for all character codes in seg
%%  uint16 * segCount : idRangeOffset[]: offset in bytes to glyph index array or 0
%%  uint16 * variable : Glyph index array
%%
%% segments are sorted in increasing endCode value
cmapf4(Tab, Off) ->
    <<_Before:Off/binary,4:16,Len:16,_Lang:16,SegcountX2:16,_SrchRng:16,
      _EntSel:16,_RngSh:16,
      BEnds:SegcountX2/binary,_Pad:16,BStarts:SegcountX2/binary,
      BDeltas:SegcountX2/binary, T/binary>> = Tab,
    N = SegcountX2 div 2,
    NGI = (Len - 8*2 - 4*SegcountX2) div 2,
    {Ends,_} = takeushorts(N, binary_to_list(BEnds)),
    {Starts,_} = takeushorts(N, binary_to_list(BStarts)),
    {Deltas,_} = takeushorts(N, binary_to_list(BDeltas)),
    {ROffsGlinds,_} = takeushorts(N+NGI, binary_to_list(T,1,SegcountX2+2*NGI)),
    docmapf4(1,N,Ends,Starts,Deltas,ROffsGlinds,0,[]).

docmapf4(I,N,_Ends,_Starts,_Deltas,_ROffsGlinds,Alen,Acc)
  when (I > N) or (Alen >= 256) ->
    fincmapf4(Acc,Alen);
docmapf4(I,N,Ends,Starts,Deltas,ROffsGlinds,Alen,Acc) ->
    E = element(I,Ends),
    S = element(I,Starts),
    D = element(I,Deltas),
    R = element(I,ROffsGlinds),
    E2 = if E > 255 -> 255; true -> E end,
    S2 = if S >= Alen -> S; true -> Alen end,
    case (S2 >= 256) or (S2 > E2) of
	true -> fincmapf4(Acc,Alen);
	false ->
	    Padlen = S2 - Alen,
	    Pad = lists:duplicate(Padlen, 0),
	    Mplen = E2-S2+1,
	    Mpart =
		case R of
		    0 -> cmapf4r0(E2,S2,D);
		    16#FFFF -> lists:duplicate(Mplen,0);
		    _ -> cmapf4rx(E2,S2,D,R,I,ROffsGlinds)
		end,
	    Acc2 = lists:append([Acc,Pad,Mpart]),
	    Alen2 = Alen + Padlen + Mplen,
	    docmapf4(I+1,N,Ends,Starts,Deltas,ROffsGlinds,Alen2,Acc2)
    end.

fincmapf4(Acc,Alen) when Alen == 256 -> list_to_tuple(Acc);
fincmapf4(Acc,Alen) when Alen < 256 ->
    Padlen = 256 - Alen,
    list_to_tuple(Acc ++ lists:duplicate(Padlen,0));
fincmapf4(Acc,Alen) when Alen > 256 ->
    list_to_tuple(lists:sublist(Acc, 1, 256)).

%% offset 0 case of format 4: just add D to [S, S+1, ..., E]
%% to get mapped glyphs.
cmapf4r0(E,S,D) -> [ ushortmod(K+D) || K <- lists:seq(S,E) ].

%% offset !0 case of format 4: have to look at offset table and glyph
%% index table as concatenated; add offset (as byte count) to current
%% address of current place in the offset table, then look at the
%% ushort there, and if not zero, add delta to it to get mapped glyph
%% for S.  Continue through until get mapped glyph for E.
cmapf4rx(E,S,D,R,I,T) when S =< E ->
    J = I + (R div 2),
    IDXS = [ element(J+K, T) || K <- lists:seq(0,E-S) ],
    map(fun (A) -> if A == 0 -> 0; true -> ushortmod(A+D) end end, IDXS);
cmapf4rx(_,_,_,_,_,_) -> [].

ushortmod(X) -> X rem 65536.

%% Parse the "head" (Font Header) tab of Tabs and return {units-per-em, shortloca}
%% where shortloca is true if loca table uses "short" format
parseheadtab(Tabs) ->
    Tab = findtab("head", Tabs),
    <<16#00010000:32,_Frev:32,_Csuma:32,16#5F0F3CF5:32,
      _Flags:16,Uperem:16,_Dcreat:64,_Dmod:64,
      _Xmin:16,_Ymin:16,_Xmax:16,_Ymax:16,
      _Macsty:16,_LRP:16,_Fdir:16,IndToLocFmt:16,0:16>> = Tab,
    {Uperem, case IndToLocFmt of 0 -> true; _ -> false end}.

%% Parse the "loca" tab of Tabs and return an (Nglyph+1)-element tuple
%% mapping a glyph index into an offset in the glyf table.
%% ShortLoca is true for the "short" format, false otherwise.
parselocatab(Tabs, Nglyph, ShortLoca) ->
    Tab = findtab("loca", Tabs),
    case ShortLoca of
	true ->
	    locashort(Nglyph+1,Tab,[]);
	false ->
	    localong(Nglyph+1,Tab,[])
    end.

%% short format: unsigned shorts divided by two are in table
locashort(0,_,Acc) ->
    list_to_tuple(reverse(Acc));
locashort(N,<<X:16/unsigned,T/binary>>,Acc) ->
    locashort(N-1, T, [2*X|Acc]).

localong(0,_,Acc) ->
    list_to_tuple(reverse(Acc));
localong(N,<<X:32,T/binary>>,Acc) ->
    localong(N-1, T, [X|Acc]).

%% Parse the "hhea" (Horizontal Header) tab of Tabs and return numberOfHMetrics
parsehheatab(Tabs) ->
    Tab = findtab("hhea", Tabs),
    <<16#00010000:32,_Asc:16,_Desc:16,_Lgap:16,_Awmax:16,
      _Minlsb:16,_Minrsb:16,_Xmaxext:16,_Cslrise:16,_Cslrun:16,
      _Res:10/binary, 0:16, NumberOfHMetrics:16/unsigned>> = Tab,
    NumberOfHMetrics.

%% Parse the "hmtx" (Horizontal Metrics) tab of Tabs and return an Nglyph-element tuple
%% mapping a glyph index into the amound (in FUnits) to advance in the x-direction
%% after "printing" the glyph.
parsehmtxtab(Tabs, Nglyph, Nhmetrics) ->
    Tab = findtab("hmtx", Tabs),
    hmtx(Nglyph, Nhmetrics, Tab, []).

%% need to repeat last element if Nhmetrics goes to zero before Nglyph
hmtx(0, _, _, Acc) ->
    list_to_tuple(reverse(Acc));
hmtx(Nglyph, Nhmetrics, <<Aw:16/unsigned,_Lsb:16,T/binary>>, Acc) ->
    Acc1 = [Aw | Acc],
    Ng1 = Nglyph-1,
    Nh1 = Nhmetrics-1,
    if
	Nh1 == 0, Ng1 > 0 ->
	    list_to_tuple(reverse(Acc) ++ duplicate(Ng1, Aw));
	true ->
	    hmtx(Ng1, Nh1, T, Acc1)
    end.

getpolyareas(Text, Ttf, Nsubsteps) ->
    Pas = getpolyareas(Text, Ttf, 0, []),
    Pas1 = clean_pas(Pas),
    subdivide_pas(Pas1, Nsubsteps).

getpolyareas([], _, _, Acc) ->
    flatten(reverse(Acc));
getpolyareas([C|Rest], #ttfont{nglyph=Ng,adv=Adv,cmap=Cmap}=Ttf, X, Acc) ->
    {X1,Acc1} =
	case (C >= 0) and (C < 256) of
	    true ->
		G = element(C+1, Cmap),
		if
		    G < Ng ->
			Xnew = X + element(G+1, Adv),
			case glyphpolyareas(G, Ttf, X) of
			    nil ->
				{Xnew, Acc};
			    Pa ->
				{Xnew, [Pa|Acc]}
			end;
		    true ->
			{X, Acc}
		end;
	    false ->
		{X, Acc}
	end,
    getpolyareas(Rest, Ttf, X1, Acc1).

%% Get contours for glyph G (known to be in range 0..nglyph-1).
%% Return nil if no data or no contours for glyph G.
%%
%% Format of glyph data:
%%  uint16 - number of contours (-1 means this is made of other chars, 0 means no data)
%%  FWord - xmin
%%  FWord - ymin
%%  FWord - xmax
%%  FWord - ymax
%%  then comes data for simple or compound glyph

glyphpolyareas(G, #ttfont{loca=Loca,glyf=Glyf,uperem=Uperem}, X) ->
    Off = element(G+1, Loca),
    Len = element(G+2, Loca) - Off,
    if
	Len < 9 ->
	    nil;
	true ->
	    Gdat = binary_to_list(Glyf, Off+1, Off+Len),
	    [Nch,Ncl|T1] = Gdat,
	    Ncont = toushort(Nch, Ncl),
	    if
		Ncont == 0 ->
		    nil;
		Ncont == 65535 ->
		    nil;
		true ->
		    %% Calculate scale so Em box measures 2 by 2
		    %% (about the scale of wings primatives)
		    Scale = 2.0/float(Uperem),
		    gpa(nthtail(4*2, T1), Ncont, X, Scale)
	    end
    end.

%% continue glyphpolyareas, when there are > 0 contours
%% (Gdat is now at start of endPtsOfContours array)
%% format expected for Gdat:
%%  uint16 * number_of_contours : endPtsOfContours array (entries are point indices)
%%  (the total number of points is one more than last entry in that array)
%%  uint16 : instruction length, the number of bytes used for instructions
%%  uint8 * instruction_length : the instructions for this glyph
%%  uint8 * (variable) : flags array: one per point, or less, if repeats
%%  (uint8 | uint16) sequence : x coordinates (each could be one or two bytes or same as prev)
%%  (uint8 | uint16) sequence : y coordinates (each could be one or two bytes or same as prev)
%%
%% flag bits:
%%  0 (1) : on curve (if set, point is on curve, else it's off)
%%  1 (2) : x-short (if set, x coord is one byte and x-same bit gives sign,
%%                  %  else x coord is two bytes unless x-same bit is set - then xcoord
%%                  %  is omitted because it is same as previous x coord)
%%  2 (4) : y-short
%%  3 (8) : repeat (if set, next byte gives count: repeat this flag count times after)
%%  4 (16): x-same (used in conjunction with x-short)
%%  5 (32): y-same

gpa(Gdat, Ncont, Xorg, Scale) ->
    {Eoc,T1} = takeushorts(Ncont,Gdat),
    Npt = element(Ncont, Eoc)+1,
    [Ninstrh,Ninstrl | T2] = T1,
    Ninstr = toushort(Ninstrh,Ninstrl),
    T3 = nthtail(Ninstr,T2),
    {Flags,T4} = gflags(Npt, T3),
    {X0,T5} = gcoords(Npt, T4, Flags, 2, 16),
    {Y0,_} = gcoords(Npt, T5, Flags, 4, 32),
    X = makeabs(X0, Xorg, Scale),
    Y = makeabs(Y0, 0, Scale),
    Cntrs = contours(Ncont, Eoc, X, Y, Flags),
    Ccntrs = map(fun getcedges/1, Cntrs),
    Ccntrs1 = lists:filter(fun (V) -> length(V) > 2 end, Ccntrs),
    findpolyareas(Ccntrs1).

%% For debugging
%% dumpsvg(X,Y) -> dsvg(X,Y,[],[], 0).
%% dsvg([],[],Plist,Vlist,_) ->
%%	Vtab = list_to_tuple(reverse(Vlist)),
%%	Ps = [reverse(Plist)],
%%	wpc_svg:write_polys("try.svg", Ps, Vtab);
%% dsvg([X1 | XR], [Y1 | YR], Pl, Vl, I) ->
%%	dsvg(XR, YR, [I|Pl], [{X1,Y1}|Vl], I + 1).

%% Take N pairs of bytes off of L, convert each pair to ushort,
%% return {tuple of the ushorts, remainder of L}.
takeushorts(N,L) -> takeushorts(N,L,[]).

takeushorts(0, L, Acc) ->
    {list_to_tuple(reverse(Acc)), L};
takeushorts(N, [B1,B2 | Rest], Acc) ->
    takeushorts(N-1, Rest, [toushort(B1,B2) | Acc]).

%% Get N glyph flags from L and return {list of flags, rest of L}.
%% Less than N flags might come off of L because if a flag has the
%% repeat bit (8) set, the next byte is used as a repeat count.
gflags(N,L) -> gflags(N,L,[]).

gflags(0, L, Acc) ->
    {reverse(Acc), L};
gflags(N, [F|Rest], Acc) ->
    Acc1 = [F | Acc],
    if
	(F band 8) == 8 ->	%% repeat F next-byte more times
	    [Rep|Rest2] = Rest,
	    Acc2 = duplicate(Rep,F) ++ Acc1,
	    gflags(N-1-Rep, Rest2, Acc2);
	true ->
	    gflags(N-1, Rest, Acc1)
    end.

%% Get N glyph coords from L and return {list of coords, rest of L}.
%% The coords are relative-to-previous at this point.
%% The Flags list controls how next coord comes off of L:
%% if Sbit is set, it's one byte (and Rbit is set if positive), else 2 bytes.
%% if Sbit isn't set, Rbit set means value is same as previous (relative offset = 0)
gcoords(N,L,Flags,Sbit,Rbit) -> gcoords(N,L,Flags,Sbit,Rbit,[]).

gcoords(0,L,_,_,_,Acc) ->
    {reverse(Acc), L};
gcoords(N,L,[F|Tf],Sbit,Rbit,Acc) ->
    SRbits = Sbit bor Rbit,
    case F band SRbits of
	0 ->
	    [B1,B2|Tl] = L,
	    gcoords(N-1, Tl, Tf, Sbit, Rbit, [tosshort(B1,B2)|Acc]);
	SRbits ->
	    [B|Tl] = L,
	    gcoords(N-1, Tl, Tf, Sbit, Rbit, [B|Acc]);
	Sbit ->
	    [B|Tl] = L,
	    gcoords(N-1, Tl, Tf, Sbit, Rbit, [-B|Acc]);
	Rbit ->
	    gcoords(N-1, L, Tf, Sbit, Rbit, [0|Acc])
    end.

toushort(B1,B2) -> B1*256 + B2.

tosshort(B1,B2) ->
    <<A:16/signed>> = list_to_binary([B1,B2]),
    A.

%% Change coords in L to be absolute (starting at V) rather than relative.
%% Also, after translation, make into a float and scale by Scale
makeabs(L, V, Scale) ->
    {Labs, _} = mapfoldl(fun (Z,Pos) ->
				 Znew = Z+Pos,
				 {Scale*float(Znew),Znew}
			 end, V, L),
    Labs.

%% Return list of Ncont {list of x-coords, list of y-coords, flags} tuples,
%% where each is a sublist of X, Y, Flags, as directed by Eoc tuple.
contours(Ncont, Eoc, X, Y, Flags) -> contours(Ncont, 1, 1, Eoc, X, Y, Flags, []).

contours(0, _, _, _, _, _, _, Acc) ->
    reverse(Acc);
contours(Ncont, I, Start, Eoc, X, Y, Flags, Acc) ->
    End = element(I, Eoc) + 1,
    Len = End - Start + 1,
    X1 = sublist(X, Start, Len),
    Y1 = sublist(Y, Start, Len),
    F1 = sublist(Flags, Start, Len),
    contours(Ncont-1, I+1, End+1, Eoc, X, Y, Flags, [{X1,Y1,F1}|Acc]).

%% Turn the parallel lists (X,Y,Flags), representing a TrueType glyph,
%% into a list of cedges.
%% We have to turn a quadratic B-spline into a list of cubic bezier curves.
getcedges({X,Y,Flags}) ->
    N = length(X),
    if
	N >= 3 ->
	    getcedges(X, Y, Flags, X, Y, Flags, []);
	true ->
	    []
    end.

%% Quadratic B-spline edges go between "on-curve" points with one
%% intermediate "off-curve" control point. But if the points don't
%% alternate on-off-on-off..., you can derive the missing ones by
%% averaging their neighbors.
%% Looking at the on-curve/off-curve status of the head three points
%% on the lists, there are six cases to worry about:
%%  i    i+1   i+2
%% a.   off    on    -
%%  .........+           dotted line shows edge assumed previously added
%%
%% b.   on    off   on     qedge {i,i+1,i+2}
%%  +______o_____+     underscore shows edge between + points, with 'o' ctl
%%
%% c.   on    off   off    qedge {i,I+1,avg(i+1,i+2)}
%%  +_____o___*        edge goes to interpolated '*' point
%%
%% d.   on    on     -     line {i,i+1}
%%      +_____+            straight edge
%%
%% e.   off   off   on     qedge {avg(i,i+1),i+1,i+2}
%%    ......*__o_____+     incoming edge assumed previously added
%%
%% f.   off   off   off    qedge {avg(i,i+1),i+1,avg(i+1,i+2)}
%%    ......*__o__*

getcedges([], [], [], _, _, _, Acc) ->
    reverse(Acc);
getcedges([_|Xt]=X, [_|Yt]=Y, [_|Ft]=Flags, Xo, Yo, Fo, Acc) ->
    {Cur,Ison} = nthptandison(1, X, Y, Flags, Xo, Yo, Fo),
    {Next,Isnexton} = nthptandison(2, X, Y, Flags, Xo, Yo, Fo),
    {Anext,Isanexton} = nthptandison(3, X, Y, Flags, Xo, Yo, Fo),
    case (not(Ison) and Isnexton) of
	true ->
	    %% this case generates no segment
	    getcedges(Xt, Yt, Ft, Xo, Yo, Fo, Acc);
	_ ->
	    {Curon,Ctl,Nexton} =
		case {Ison,Isnexton,Isanexton} of
		    {true,false,true} -> {Cur,Next,Anext};
		    {true,false,false} -> {Cur,Next,avg(Next,Anext)};
		    {true,true,_} -> {Cur,nil,Next};
		    {false,false,true} -> {avg(Cur,Next),Next,Anext};
		    {false,false,false} -> {avg(Cur,Next),Next,avg(Next,Anext)}
		end,
	    %% Ctl, if not nil, is quadratic Bezier control point.
	    %% Following uses degree-elevation theory to get cubic cps.
	    {Cp1,Cp2} = case Ctl of
			    nil -> {nil, nil};
			    _ -> {lininterp(2.0/3.0, Curon, Ctl),
				  lininterp(2.0/3.0, Nexton, Ctl)}
			end,
	    Edge = #cedge{vs=Curon, cp1=Cp1, cp2=Cp2, ve=Nexton},
	    getcedges(Xt, Yt, Ft, Xo, Yo, Fo, [Edge|Acc])
    end.

avg({X1,Y1},{X2,Y2}) -> {0.5*(X1+X2), 0.5*(Y1+Y2)}.

lininterp(F,{X1,Y1},{X2,Y2}) -> {(1.0-F)*X1 + F*X2, (1.0-F)*Y1 + F*Y2}.

%% Return {Nth point, is-on-curve flag} based on args
%% (use (Xo,Yo,Fo), the original list, when need to wrap).
nthptandison(1, [X|_], [Y|_], [F|_], _Xo, _Yo, _Fo) ->
    {{X,Y}, if (F band 1) == 1 -> true; true -> false end};
nthptandison(N, [_|Xt], [_|Yt], [_|Ft], Xo, Yo, Fo) ->
    nthptandison(N-1, Xt, Yt, Ft, Xo, Yo, Fo);
nthptandison(N, [], _, _, Xo, Yo, Fo) ->
    nthptandison(N, Xo, Yo, Fo, Xo, Yo, Fo).

%% Cconts is list of "curved contours".
%% Each curved contour is a list of cedges, representing a closed contour.
%% This routine analyzes the contours and partitions them into polyareas,
%% where each polyarea has a boundary (CCW oriented) and an optional list
%% of contained islands (each CW oriented).
findpolyareas(Cconts) ->
    Areas = map(fun ccarea/1, Cconts),
    {Cc,_Ar} = orientccw(Cconts, Areas),
    Cct = list_to_tuple(Cc),
    N = size(Cct),
    Art = list_to_tuple(Areas),
    Lent = list_to_tuple(map(fun length/1,Cc)),
    Seqn = seq(1,N),
    Cls = [ {{I,J},classifyverts(element(I,Cct),element(J,Cct))}
	    || I <- Seqn, J <- Seqn],
    Clsd = gb_trees:from_orddict(Cls),
    Cont = [ {{I,J},contains(I,J,Art,Lent,Clsd)}
	     || I <- Seqn, J <- Seqn],
    Contd = gb_trees:from_orddict(Cont),
    Assigned = gb_sets:empty(),
    getpas(1,N,Contd,Cct,{[],Assigned}).

getpas(I,N,Contd,Cct,{Pas,Ass}) when I > N ->
    case length(gb_sets:to_list(Ass)) of
	N ->
	    reverse(Pas);
	_ ->
	    %% not all assigned: loop again
	    getpas(1,N,Contd,Cct,{Pas,Ass})
    end;
getpas(I,N,Contd,Cct,{Pas,Ass}=Acc) ->
    case gb_sets:is_member(I,Ass) of
	true -> getpas(I+1,N,Contd,Cct,Acc);
	_ ->
	    case isboundary(I,N,Contd,Ass) of
		true ->
		    %% have a new polyarea with boundary = contour I
		    Ass1 = gb_sets:add(I,Ass),
		    {Isls,Ass2} = getisls(I,N,N,Contd,Ass1,Ass1,[]),
		    Cisls = map(fun (K) -> revccont(element(K,Cct)) end, Isls),
		    Pa = #polyarea{boundary=element(I,Cct), islands=Cisls},
		    getpas(I+1,N,Contd,Cct,{[Pa|Pas],Ass2});
		_ -> getpas(I+1,N,Contd,Cct,Acc)
	    end
    end.

%% Return true if thre is no unassigned J <= second arg, J /= I,
%% such that contour J contains contour I.
isboundary(_I,0,_Contd,_Ass) -> true;
isboundary(I,I,Contd,Ass) -> isboundary(I,I-1,Contd,Ass);
isboundary(I,J,Contd,Ass) ->
    case gb_sets:is_member(J,Ass) of
	true ->
	    isboundary(I,J-1,Contd,Ass);
	_ ->
	    case gb_trees:get({J,I},Contd) of
		true -> false;
		_ -> isboundary(I,J-1,Contd,Ass)
	    end
    end.

%% Find islands for contour I : i.e., unassigned contours directly inside it.
%% Only have to check J and less.
%% Ass, Isls are (assigned-so-far, islands-so-far).
%% Ass0 is assigned before we started adding islands.
%% Return {list of island indices, Assigned array with those indices added}
getisls(_I,0,_N,_Contd,_Ass0,Ass,Isls) -> {reverse(Isls),Ass};
getisls(I,J,N,Contd,Ass0,Ass,Isls) ->
    case gb_sets:is_member(J,Ass) of
	true ->
	    getisls(I,J-1,N,Contd,Ass0,Ass,Isls);
	_ ->
	    case directlycont(I,J,N,Contd,Ass0) of
		true ->
		    getisls(I,J-1,N,Contd,Ass0,gb_sets:add(J,Ass),[J|Isls]);
		_ ->
		    getisls(I,J-1,N,Contd,Ass0,Ass,Isls)
	    end
    end.

directlycont(I,J,N,Contd,Ass) ->
    gb_trees:get({I,J},Contd) andalso
	foldl(fun (K,DC) ->
		      DC andalso
			   (K == J orelse gb_sets:is_member(K,Ass) orelse
			    not(gb_trees:get({K,J},Contd))) end,
	      true, seq(1,N)).

ccarea(Ccont) ->
    0.5 * foldl(fun (#cedge{vs={X1,Y1},ve={X2,Y2}},A) ->
			A + X1*Y2 - X2*Y1 end,
		0.0, Ccont).

%% Reverse contours if area is negative (meaning they were Clockwise),
%% and return revised Cconts and Areas.
orientccw(Cconts, Areas) -> orientccw(Cconts, Areas, [], []).

orientccw([], [], Cacc, Aacc) ->
    { reverse(Cacc), reverse(Aacc) };
orientccw([C|Ct], [A|At], Cacc, Aacc) ->
    if
	A >= 0.0 ->
	    orientccw(Ct, At, [C|Cacc], [A|Aacc]);
	true ->
	    orientccw(Ct, At, [revccont(C)|Cacc], [-A|Aacc])
    end.

revccont(C) -> reverse(map(fun revcedge/1, C)).

%% reverse a cedge
revcedge(#cedge{vs=Vs,cp1=Cp1,cp2=Cp2,ve=Ve}) ->
    #cedge{vs=Ve,cp1=Cp2,cp2=Cp1,ve=Vs}.

%% classify vertices of contour B with respect to contour A.
%% return {# inside A, # on A}.
classifyverts(A,B) -> foldl(fun (#cedge{vs=Vb},Acc) -> cfv(A,Vb,Acc) end,
			    {0,0}, B).

%% Subdivide (bisect each each) Nsubsteps times.
%% When bezier edges are subdivided, the inserted point goes
%% at the proper place on the curve.
subdivide_pas(Pas,0) -> Pas;
subdivide_pas(Pas,Nsubsteps) ->
    map(fun (Pa) -> subdivide_pa(Pa,Nsubsteps) end, Pas).

subdivide_pa(Pa, 0) ->
    Pa;
subdivide_pa(#polyarea{boundary=B,islands=Isls}, N) ->
    subdivide_pa(#polyarea{boundary=subdivide_contour(B),
			   islands=map(fun subdivide_contour/1, Isls)}, N-1).

subdivide_contour(Cntr) ->
    flatten(map(fun (CE) -> subdivide_cedge(CE,0.5) end, Cntr)).

%% subdivide CE at parameter Alpha, returning two new CE's in list.
subdivide_cedge(#cedge{vs=Vs,cp1=nil,cp2=nil,ve=Ve},Alpha) ->
    Vm = lininterp(Alpha, Vs, Ve),
    [#cedge{vs=Vs,ve=Vm}, #cedge{vs=Vm,ve=Ve}];
subdivide_cedge(#cedge{vs=Vs,cp1=C1,cp2=C2,ve=Ve},Alpha) ->
    B0 = {Vs,C1,C2,Ve},
    B1 = bezstep(B0,1,Alpha),
    B2 = bezstep(B1,2,Alpha),
    B3 = bezstep(B2,3,Alpha),
    [#cedge{vs=element(1,B0),cp1=element(1,B1),cp2=element(1,B2),ve=element(1,B3)},
     #cedge{vs=element(1,B3),cp1=element(2,B2),cp2=element(3,B1),ve=element(4,B0)}].

bezstep(B,R,Alpha) ->
    list_to_tuple(bzss(B,0,3-R,Alpha)).

bzss(_B,I,Ilim,_Alpha) when I > Ilim -> [];
bzss(B,I,Ilim,Alpha) ->
    [lininterp(Alpha,element(I+1,B),element(I+2,B)) | bzss(B,I+1,Ilim,Alpha)].

%% Clean up all the polygons in the polyarea list Pas.
%% "Clean" means remove zero-length edges.
clean_pas(Pas) -> map(fun clean_pa/1, Pas).

clean_pa(#polyarea{boundary=B,islands=Isls}) ->
    #polyarea{boundary=clean_contour(B),
	      islands=map(fun clean_contour/1, Isls)}.

clean_contour([]) -> [];
clean_contour([CE=#cedge{vs=Vs,ve=Ve} | T]) ->
    case Vs==Ve of
	true -> clean_contour(T);
	_ -> [CE | clean_contour(T)]
    end.

%% Decide whether vertex P is inside or on (as a vertex) contour A,
%% and return modified pair.  Assumes A is CCW oriented.
%% CF Eric Haines ptinpoly.c in Graphics Gems IV
cfv(A,P,{Inside,On}) ->
    #cedge{vs=Va0} = last(A),
    if
	Va0 == P ->
	    {Inside, On+1};
	true ->
	    Yflag0 = (element(2,Va0) > element(2,P)),
	    case vinside(A, Va0, P, false, Yflag0) of
		true -> {Inside+1, On};
		false -> {Inside, On};
		on -> {Inside, On+1}
	    end
    end.

vinside([], _V0, _P, Inside, _Yflag0) ->
    Inside;
vinside([#cedge{vs={X1,Y1}=V1}|Arest], {X0,Y0}, P={Xp,Yp}, Inside, Yflag0) ->
    if
	V1 == P ->
	    on;
	true ->
	    Yflag1 = (Y1 > Yp),
	    Inside1 =
		if
		    Yflag0 == Yflag1 -> Inside;
		    true ->
			Xflag0 = (X0 >= Xp),
			Xflag1 = (X1 >= Xp),
			if
			    Xflag0 == Xflag1 ->
				case Xflag0 of
				    true -> not(Inside);
				    _ -> Inside
				end;
			    true ->
				Z = X1 - (Y1-Yp)*(X0-X1)/(Y0-Y1),
				if
				    Z >= Xp -> not(Inside);
				    true -> Inside
				end
			end
		end,
	    vinside(Arest, V1, P, Inside1, Yflag1)
    end.

%% I, J are indices into tuple Cct of curved contours.
%% Clsd is gb_tree mapping {I,J} to [Inside,On,Outside].
%% Return true if contour I contains at least 55% of contour J's vertices.
%% (This low percentage is partly because we are dealing with polygonal approximations
%% to curves, sometimes, and the containment relation may seem worse than it actually is.)
%% Lengths (in Lent tuple) are used for calculating percentages.
%% Areas (in Art tuple) are used for tie-breaking.
%% Return false if contour I is different from contour J, and not contained in it.
%% Return same if I == J or all vertices on I are on J (duplicate contour).
contains(I,I,_,_,_) ->
    same;
contains(I,J,Art,Lent,Clsd) ->
    LenI = element(I,Lent),
    LenJ = element(J,Lent),
    {JinsideI,On} = gb_trees:get({I,J},Clsd),
    if
	JinsideI == 0 ->
	    false;
	On == LenJ, LenI == LenJ ->
	    same;
	true ->
	    if
		float(JinsideI) / float(LenJ) > 0.55 ->
		    {IinsideJ,_} = gb_trees:get({J,I},Clsd),
		    FIinJ = float(IinsideJ) / float(LenI),
		    if
			FIinJ > 0.55 ->
			    element(I,Art) >= element(J,Art);
			true ->
			    true
		    end;
		true ->
		    false
	    end
    end.

%% Return {Vs,Fs} where Vs is list of {X,Y,Z} for vertices 0, 1, ...
%% and Fs is list of lists, each sublist is a face (CCW ordering of
%% (zero-based) indices into Vs).
pa2object(#polyarea{boundary=B,islands=Isls}) ->
    Vslist = [cel2vec(B, 0.0) | map(fun (L) -> cel2vec(L, 0.0) end, Isls)],
    Vtop = flatten(Vslist),
    Vbot = map(fun ({X,Y,Z}) -> {X,Y,Z-0.2} end, Vtop),
    Vs = Vtop ++ Vbot,
    Nlist = [length(B) | map(fun (L) -> length(L) end, Isls)],
    Ntot = sum(Nlist),
    Fs1 = [FBtop | Holestop] = faces(Nlist,0,top),
    Fs2 = [FBbot | Holesbot] = faces(Nlist,Ntot,bot),
    Fsides = sidefaces(Nlist, Ntot),
    FtopQ = e3d__tri_quad:quadrangulate_face_with_holes(FBtop, Holestop, Vs),
    FbotQ = e3d__tri_quad:quadrangulate_face_with_holes(FBbot, Holesbot, Vs),
    Ft = [ F#e3d_face.vs || F <- FtopQ ],
    Fb = [ F#e3d_face.vs || F <- FbotQ ],
    Fs = Ft ++ Fb ++ Fsides,
    {Vs,Fs, [ F#e3d_face.vs || F <- Fs1 ++ Fs2]}.

cel2vec(Cel, Z) -> map(fun (#cedge{vs={X,Y}}) -> {X,Y,Z} end, Cel).

faces(Nlist,Org,Kind) -> faces(Nlist,Org,Kind,[]).

faces([],_Org,_Kind,Acc) -> reverse(Acc);
faces([N|T],Org,Kind,Acc) ->
    FI = case Kind of
	     top -> #e3d_face{vs=seq(Org, Org+N-1)};
	     bot -> #e3d_face{vs=seq(Org+N-1, Org, -1)}
	 end,
    faces(T,Org+N,Kind,[FI|Acc]).

sidefaces(Nlist,Ntot) -> sidefaces(Nlist,0,Ntot,[]).

sidefaces([],_Org,_Ntot,Acc) -> append(reverse(Acc));
sidefaces([N|T],Org,Ntot,Acc) ->
    End = Org+N-1,
    Fs = [ [I, Ntot+I, wrap(Ntot+I+1,Ntot+Org,Ntot+End), wrap(I+1,Org,End)]
	   || I <- seq(Org, End) ],
    sidefaces(T,Org+N,Ntot,[Fs|Acc]).

%% I should be in range (Start, Start+1, ..., End).  Make it so.
wrap(I,Start,End) -> Start + ((I-Start) rem (End+1-Start)).

offsetfaces(Fl, Offset) ->
    map(fun (F) -> offsetface(F,Offset) end, Fl).

offsetface(F, Offset) ->
    map(fun (V) -> V+Offset end, F).

