%%
%%  wpc_tt.erl --
%%
%%     Functions for reading TrueType fonts (.tt)
%%
%%  Copyright (c) 2001-2011 Howard Trickey & Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%  Rewritten to support Unicode codepoints and more complete support of
%%  the standard. /Dan
%%

-module(wpc_tt).
-export([init/0,menu/2,command/2,
         sysfontdirs/0, process_ttfs/1, trygen/3 % debugging
        ]). % for ai

-import(lists, [reverse/1,sort/2,keysearch/3,duplicate/2,nthtail/2,
		mapfoldl/3,foldl/3,sublist/3,map/2,last/1,seq/2,seq/3,
		flatten/1,sum/1,append/1]).

-include_lib("wings/src/wings.hrl").
-include_lib("wings/e3d/e3d.hrl").

-record(ttf_info,
	{num_glyphs,     %% Number of Glyphs

	 %% Offsets to table locations
	 loca, head, glyf, hhea, hmtx, kern,

	 index_map,     %% A Cmap mapping for out  chosen character encoding
	 index_to_loc_format, %% Format needed to map from glyph index to glyph
	 data          %% The binary file
	}).

-record(polyarea,
	{boundary,			%%list of cedges (CCW oriented, closed)
	 islands=[]}).			%%list of lists of cedges (CW, closed)

-record(vertex, {pos, c, type}).

%% a "possibly curved" edge, with explicit coords
%% and optional cubic bezier control points
-record(cedge,
	{vs,cp1=nil,cp2=nil,ve}).	%all are {x,y} pairs

-type ttf() :: #ttf_info{}.
%% -type scale() :: Uniform::float() | {ScaleX::float(),ScaleY::float()}.
%% -type shift() :: Uniform::float() | {ShiftX::float(),ShiftY::float()}.
%% -type size() :: {Width::integer(), Height::integer()}.
-type vertex() :: #vertex{}.
-type platform() :: unicode | mac | microsoft | integer().
-type encoding() :: unicode | roman | integer().
-type language() :: english | integer().  %% 0 if platform is unicode

-define (fsITALIC, 2#00000001).
-define (fsBOLD,   2#00100000).

%% PLATFORM ID
-define(PLATFORM_ID_UNICODE,  0).
-define(PLATFORM_ID_MAC,      1).
-define(PLATFORM_ID_ISO,      2).
-define(PLATFORM_ID_MICROSOFT,3).

%%  encodingID for PLATFORM_ID_UNICODE
-define(UNICODE_EID_UNICODE_1_0    ,0).
-define(UNICODE_EID_UNICODE_1_1    ,1).
-define(UNICODE_EID_ISO_10646      ,2).
-define(UNICODE_EID_UNICODE_2_0_BMP,3).
-define(UNICODE_EID_UNICODE_2_0_FULL,4).

%% encodingID for PLATFORM_ID_MICROSOFT
-define(MS_EID_SYMBOL        ,0).
-define(MS_EID_UNICODE_BMP   ,1).
-define(MS_EID_SHIFTJIS      ,2).
-define(MS_EID_UNICODE_FULL  ,10).

%% encodingID for PLATFORM_ID_MAC; same as Script Manager codes
-define(MAC_EID_ROMAN        ,0).
-define(MAC_EID_JAPANESE     ,1).
-define(MAC_EID_CHINESE_TRAD ,2).
-define(MAC_EID_KOREAN       ,3).
-define(MAC_EID_ARABIC       ,4).
-define(MAC_EID_HEBREW       ,5).
-define(MAC_EID_GREEK        ,6).
-define(MAC_EID_RUSSIAN      ,7).

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
    FontDirs = sysfontdirs(),
    DefFont  = default_font(),
    FontInfo = case wpa:pref_get(wpc_tt, fontname, DefFont) of
		   FI = #{type := font} -> FI;
		   _ -> DefFont
	       end,

    Text = wpa:pref_get(wpc_tt, text, "Wings 3D"),
    Bisect = wpa:pref_get(wpc_tt, bisections, 0),
    GbtFonts = process_ttfs(FontDirs),
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
    Fun = fun({dialog_preview,[T,N,{_,Ctrl},RX,RY,RZ,MX,MY,MZ,Grnd]=_Res}) ->
                  {_, FPath} = find_font_file(GbtFonts,Ctrl),
                  {preview,{shape,{text,[T,N,{fontdir,FPath},RX,RY,RZ,MX,MY,MZ,Grnd]}},St};
             (cancel) ->
                  St;
             ([T,N,{_,WxFont},RX,RY,RZ,MX,MY,MZ,Grnd]=_Res) when is_tuple(WxFont) ->
                  {NewFontI, FPath} = find_font_file(GbtFonts,WxFont),
                  case FPath of
                      undefined ->
                          St;
                      _ ->
                          wpa:pref_set(wpc_tt, fontname, NewFontI),
                          wpa:pref_set(wpc_tt, text, element(2,T)),
                          wpa:pref_set(wpc_tt, bisections, element(2,N)),
                          {commit,{shape,{text,[T,N,{fontdir,FPath},RX,RY,RZ,MX,MY,MZ,Grnd]}},St}
                  end
          end,
    wings_dialog:dialog(Ask,?__(1,"Create Text"), {preview, Dlg}, Fun);

make_text([{_,T},{_,N},{_,FontFile}|Transf], _) ->
    gen(FontFile, T, N, Transf).

help_button() ->
    Title = ?__(1,"Select font"),
    TextFun = fun () -> help() end,
    {help,Title,TextFun}.

help() ->
    [?__(1,"Only TrueType fonts can be used and they must be"
	 "installed in standard operating system directory for fonts.")].

gen(_FontFile, "", _Nsubsteps, _Transf) ->
    keep;
gen(File, Text, Nsubsteps, Transf) ->
    try trygen(File, Text, Nsubsteps) of
	{new_shape,Name,Fs0,Vs0,He} ->
	    Vs = wings_shapes:transform_obj(Transf, Vs0),
	    Fs = [#e3d_face{vs=Vsidx} || Vsidx <- Fs0],
	    Mesh = #e3d_mesh{type=polygon, vs=Vs, fs=Fs, he=He},
	    {new_shape, Name, #e3d_object{obj=Mesh}, []};
	{error,"no such file or directory"} ->
	    wpa:error_msg(?__(4,"Text failed: failed to locate the TTF file for the selected font"));
	{error,Reason} ->
	    wpa:error_msg(?__(1,"Text failed: ") ++ Reason)
    catch _:X:ST ->
	    io:format(?__(2,"caught error: ") ++"~P~nST:~p", [X, 40,ST]),
	    wpa:error_msg(?__(3,"Text failed: internal error"))
    end.

process_ttfs(Dirs) ->
    Add = fun(FileName, Acc) ->
		  case ttf_info(FileName) of
		      {FName,FStyle,FWeight} ->
			  gb_trees:enter({FName,FStyle,FWeight},FileName,Acc);
		      _ -> Acc
		  end
	  end,
    lists:foldl(fun(Dir, Tree) ->
                        filelib:fold_files(Dir, ".ttf|.TTF", true, Add, Tree)
                end, gb_trees:empty(), Dirs).

ttf_info(File) ->
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
find_font_file(GbtFonts, WxFont) ->
    FontInfo = wings_text:get_font_info(WxFont),
    #{face:=FName, style:=FStyle, weight:=FWeight} = FontInfo,
    File = find_font_file_0(GbtFonts,FName,FStyle,FWeight),
    io:format("~p => ~p~n", [FontInfo, File]),
    {FontInfo, File}.

find_font_file_0(GbtFonts,FName,FStyle,FWeight) ->  % try to get the right fount
    case gb_trees:lookup({FName,FStyle,FWeight},GbtFonts) of
	{value,FPath} -> FPath;
        _ -> find_font_file_1(GbtFonts,FName,normal,FWeight)
    end.
find_font_file_1(GbtFonts,FName,FStyle,FWeight) ->  % try to get the right fount
    case gb_trees:lookup({FName,FStyle,FWeight},GbtFonts) of
	{value,FPath} -> FPath;
        _ -> find_font_file_2(GbtFonts,FName,FStyle,normal)
    end.
find_font_file_2(GbtFonts,FName,FStyle,FWeight) ->  % try to get the right fount
    case gb_trees:lookup({FName,FStyle,FWeight},GbtFonts) of
        {value,FPath} -> FPath;
        _ -> win_font_substitutes(FName,GbtFonts)
    end.

win_font_substitutes(FName,GbtFonts) ->
    case winregval("FontSubstitutes",FName) of
        none -> undefined;
        FSName ->
            case gb_trees:lookup({FSName,normal,normal},GbtFonts) of
                {value, FPath} -> FPath;
                _ -> undefined
            end
    end.

trygen(File, Text, Nsubsteps) ->
    case file:read_file(File) of
	{ok,Filecontents} ->
	    case ttfpart(Filecontents) of
		{ok, TTFpart} ->
                    {ok, TTF} = init_font(TTFpart),
		    Pa0 = get_polyareas(Text, TTF, Nsubsteps),
                    {Vs0,Fs,He} = polyareas_to_faces(Pa0),
                    %% {CX,CY,CZ} = e3d_vec:average(tuple_to_list(e3d_bv:box(Vs0))),
		    %% Vs = [{X-CX,Y-CY,Z-CZ} || {X,Y,Z} <- Vs0],
                    Res = {new_shape,"text: " ++ Text,Fs,Vs0,He},
                    %% io:format("Res: ~250.P~n",[Res, 20]),
                    Res;
		_ -> {error, ?__(1,"Can't find TrueType section in ") ++ File}
	    end;
	{error,Reason} ->
	    {error,file:format_error(Reason)}
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

%% Try to find default system directory for fonts
sysfontdirs() ->
    sysfontdirs(os:type()).

sysfontdirs({win32,Wintype}) ->
    Def = case Wintype of
              nt -> "C:/winnt";
              _ -> "C:/windows"
          end,
    SR = case winregval("", "SystemRoot") of
             none -> Def;
             Val -> Val
         end,
    [SR ++ "/Fonts"];
sysfontdirs({unix,darwin}) ->
    Home = os:getenv("HOME"),
    ["/Library/Fonts", filename:join(Home, "Library/Fonts")];
sysfontdirs({unix,_}) ->
    Home = os:getenv("HOME"),
    ["/usr/share/fonts/", "/usr/local/share/fonts",
     filename:join(Home, ".fonts"),
     filename:join(Home, ".local/share/fonts")].

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

is_ttf(Bin, V) ->
    is_font(Bin) andalso (not V orelse is_ttf_fin(Bin)).

is_ttf_fin(<<_:32, NumTabs:?U16, SrchRng:?U16, _EntSel:?U16, Rsh:?U16, B/binary>>) ->
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


get_polyareas(Text, Font, Nsubsteps) ->
    Scale = scale_for_mapping_em_to_pixels(Font, 2.0),
    Area = fun(CodePoint, {X,Acc}) ->
                   Glyph = find_glyph_index(Font, CodePoint),
                   {Advance, _} = get_glyph_h_metrics(Font, Glyph),
                   Areas = get_polyarea(Glyph, X, Scale, Nsubsteps, Font),
                   %% io:format("~p:~p: Char: ~c~n  ~150.p~n",[?MODULE,?LINE, CodePoint, Areas]),
                   {X+Advance, Areas ++ Acc}
           end,
    {_, Pas} = lists:foldl(Area, {0, []}, Text),
    Pas.

get_polyarea(Glyph, X, Scale, Nsubsteps, Font) ->
    GlyphVs = get_glyph_shape(Font, Glyph),
    VsCont = verts_to_point_lists(GlyphVs, Scale, Nsubsteps),
    {X0,Y0,_X1,_Y1} = get_glyph_box(Font, Glyph),
    %% io:format("~p:~p: ~p ~p~n",[?MODULE,?LINE, Scale, get_glyph_box(Font, Glyph)]),

    Make = fun(Vs) -> make_edges(Vs, {Scale,Scale}, {(X+X0)*Scale, Y0*Scale}, []) end,
    Edges = lists:map(Make, VsCont),
    %% io:format("~p:~p: Edges: ~n  ~w~n",[?MODULE,?LINE, Edges]),
    findpolyareas(Edges).

verts_to_point_lists(Vs, Scale, SubDiv) ->
    F = {4.0/(Scale*math:pow(4,SubDiv)), SubDiv},
    lists:reverse(verts_to_points(Vs, {0.0,0.0}, F, [], [])).

verts_to_points([#vertex{type=move,pos=Point}|Vs], _, F, Cont, All) ->
    verts_to_points(Vs, Point, F, [Point], add_contour(Cont, All));
verts_to_points([#vertex{type=line,pos=Point}|Vs], _, F, Cont, All) ->
    verts_to_points(Vs, Point, F, [Point|Cont], All);
verts_to_points([#vertex{type=curve,pos=VP={PX,PY},c={CX,CY}}|Vs],
                {X,Y}, {Limit,Level}=F, Cont0, All) ->
    Cont = tesselate(X,Y, CX,CY, PX,PY, Limit, Level, Cont0),
    verts_to_points(Vs, VP, F, Cont, All);
verts_to_points([], _, _, Cont, All) ->
    add_contour(Cont, All).

add_contour([], All) -> All;
add_contour(Cont, All) ->
    [lists:reverse(Cont)|All].

tesselate(X0,Y0, X1,Y1, X2,Y2, Limit, Level, Cont0) when Level >= 0 ->
    Mx = (X0 + 2*X1 + X2)/4.0,
    My = (Y0 + 2*Y1 + Y2)/4.0,
    %% Versus Directly Drawn Line
    Dx = (X0+X2)/2.0 - Mx,
    Dy = (Y0+Y2)/2.0 - My,
    io:format(" ~p,~p ~p,~p => ~p > ~p~n",[X0,Y0,X1,Y1,Dx*Dx+Dy*Dy,Limit]),
    if (Dx*Dx+Dy*Dy) > Limit ->
	    Cont1 = tesselate(X0,Y0, (X0+X1)/2.0,(Y0+Y1)/2.0, Mx,My, Limit, Level-1, Cont0),
	    tesselate(Mx,My, (X1+X2)/2.0,(Y1+Y2)/2.0, X2,Y2, Limit, Level-1, Cont1);
       true ->
	    [{X2,Y2}|Cont0]
    end;
tesselate(_X0,_Y0, _X1,_Y1, X2,Y2, _F, _Level, Cont) ->
    [{X2,Y2}|Cont].

make_edges([{JX,JY}|Rest=[{KX,KY}|_]], Scale = {ScX, ScY}, Shift = {ShX,ShY}, Eds) ->
    Edge = #cedge{vs={JX * ScX + ShX, JY * ScY + ShY},
                  ve={KX * ScX + ShX, KY * ScY + ShY}},
    case Edge of
        #cedge{vs=V,ve=V} ->  %% Remove zero size edges
            make_edges(Rest, Scale, Shift, Eds);
        _ ->
            make_edges(Rest, Scale, Shift, [Edge|Eds])
    end;
make_edges(_, _, _, Eds) ->
    lists:reverse(Eds).

%%%

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

%%%%%%%%%%%%%%%%%%%%% TTF PARSER %%%%%%%%%%%%%%%%%%%%


%% Heavily inspired from Sean Barret's code @ nothings.org (see stb_truetype.h)
%%
%% @doc
%%      Codepoint
%%         Characters are defined by unicode codepoints, e.g. 65 is
%%         uppercase A, 231 is lowercase c with a cedilla, 0x7e30 is
%%         the hiragana for "ma".
%%
%%      Glyph
%%         A visual character shape (every codepoint is rendered as
%%         some glyph)
%%
%%      Glyph index
%%         A font-specific integer ID representing a glyph
%%
%%      Baseline
%%         Glyph shapes are defined relative to a baseline, which is the
%%         bottom of uppercase characters. Characters extend both above
%%         and below the baseline.
%%
%%      Current Point
%%         As you draw text to the screen, you keep track of a "current point"
%%         which is the origin of each character. The current point's vertical
%%         position is the baseline. Even "baked fonts" use this model.
%%
%%      Vertical Font Metrics
%%         The vertical qualities of the font, used to vertically position
%%         and space the characters. See docs for get_font_v_metrics.
%%
%%      Font Size in Pixels or Points
%%         The preferred interface for specifying font sizes in truetype
%%         is to specify how tall the font's vertical extent should be in pixels.
%%         If that sounds good enough, skip the next paragraph.
%%
%%         Most font APIs instead use "points", which are a common typographic
%%         measurement for describing font size, defined as 72 points per inch.
%%         truetype provides a point API for compatibility. However, true
%%         "per inch" conventions don't make much sense on computer displays
%%         since they different monitors have different number of pixels per
%%         inch. For example, Windows traditionally uses a convention that
%%         there are 96 pixels per inch, thus making 'inch' measurements have
%%         nothing to do with inches, and thus effectively defining a point to
%%         be 1.333 pixels. Additionally, the TrueType font data provides
%%         an explicit scale factor to scale a given font's glyphs to points,
%%         but the author has observed that this scale factor is often wrong
%%         for non-commercial fonts, thus making fonts scaled in points
%%         according to the TrueType spec incoherently sized in practice.
%%

%% Initiates (and optionally reads from file) a ttf font.
%%
%% Each .ttf/.ttc file may have more than one font. Each font has a
%% sequential index number starting from 0. A regular .ttf file will
%% only define one font and it always be at index 0.

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
%info(15) -> reserved;
info(16) -> preferred_family;
info(17) -> preferred_subfamily;
%%info(18) -> compatible_full; %% Mac only
info(19) -> sample_text;
info(Id) -> Id.

-spec init_font(Font|FileName) ->
	  {ok, ttf()} | {error, term()} when
      Font :: binary(), FileName :: list().
init_font(Bin) ->
    init_font(Bin, []).

-spec init_font(Font|FileName, [Option]) ->
	{ok, ttf()} | {error, term()} when
      Font :: binary(),
      FileName :: list(),
      Option :: {index, integer()}.
init_font(Bin, Opts) when is_binary(Bin) ->
    init_font_1(Bin, Opts);
init_font(Filename, Opts) ->
    case file:read_file(Filename) of
	{ok, Bin} -> init_font_1(Bin, Opts);
	Error -> Error
    end.

init_font_1(Bin0, Opts) ->
    Index = proplists:get_value(index, Opts, 0),
    Bin  = get_font_from_offset(Bin0, Index),
    is_font(Bin) orelse throw(bad_ttf_file),
    CMap = find_table(Bin, <<"cmap">>),
    Loca = find_table(Bin, <<"loca">>),
    Head = find_table(Bin, <<"head">>),
    Glyf = find_table(Bin, <<"glyf">>),
    Hhea = find_table(Bin, <<"hhea">>),
    Hmtx = find_table(Bin, <<"hmtx">>),
    Kern = find_table(Bin, <<"kern">>),
    case [W || W <- [CMap, Loca, Head, Glyf, Hhea, Hmtx], W =:= false] of
        [false|_] -> throw(bad_ttf_file);
        _ ->ok
    end,
    NumGlyphs = num_glyphs(Bin),
    IndexMap  = find_cmap(CMap, Bin),
    Skip = Head+50,
    <<_:Skip/binary, LocFormat:?U16, ?SKIP>> = Bin,
    {ok, #ttf_info{data = Bin,  num_glyphs = NumGlyphs,
                   loca = Loca, head = Head,
                   glyf = Glyf, hhea = Hhea,
                   hmtx = Hmtx, kern = Kern,
                   index_map = IndexMap,
                   index_to_loc_format = LocFormat
                  }}.

parsett_header(_File, Bin) ->
    FontInfo = font_info(Bin),
    Family = proplists:get_value(family, FontInfo, undefined),
    {Style, Weight} = font_styles(Bin),
    %% io:format("File: ~p ~p ~p ~p~n", [_File,Family, Style, Weight]),
    {Family,Style,Weight}.

check_enc(A, A) -> true;
check_enc({unicode,_}, unicode) -> true;
check_enc({unicode,_,_}, unicode) -> true;
check_enc(_, _) -> false.

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

%% Return the requested string from font
%% By default font family and subfamily (if not regular)
-spec font_info(Font::ttf()) -> string().
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
    end.

%% Return the requested string from font
%% Info Items: 1,2,3,4,16,17 may be interesting
%% Returns a list if the encoding is known otherwise a binary.
%% Return the empty list is no info that could be matched is found.
-spec font_info(Font::ttf(),
		[InfoId::integer()],
		Platform::platform(),
		Encoding::encoding(),
		Language::language()) -> [{InfoId::integer, string()}].
font_info(#ttf_info{data=Bin}, Id, Platform, Encoding, Language) ->
    font_info(Bin, Id, Platform, Encoding, Language);
font_info(Bin, Id, Platform, Encoding, Language) when is_binary(Bin) ->
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

get_font_from_offset(Bin, 0) -> Bin;
get_font_from_offset(Bin, Index) ->
    is_font(Bin) andalso exit(not_a_font_collection),
    Skip = Index * 14,
    case Bin of
	<<"ttcf", 0,V,0,0, N:32, _:Skip/binary, FontPos:32, ?SKIP >>
	  when V =:= 1, V=:= 2 ->
	    (Index < N) orelse exit(bad_font_index),
	    <<_:FontPos/binary, FontBin/binary>> = Bin,
	    FontBin;
	_ ->
	    exit(not_a_supported_font_collection)
    end.

%% ttf fonts start with an "offset subtable":
%%  uint32 - tag to mark as TTF (one of the 0,1,0,0; "true"; or "OTTO")
%%  uint16 - number of directory tables
%%  uint16 - search range: (maximum power of 2 <= numTables)*16
%%  uint16 - entry selector: log2(maximum power of 2 <= numTables)
%%  uint16 - range shift: numTables*16-searchRange

is_font(<<1,0,0,0,?SKIP>>) -> true;  %% Truetype 1
is_font(<<"typ1",?SKIP>>)  -> true;  %% Truetype with type 1 font, not supported
is_font(<<"OTTO",?SKIP>>)  -> true;  %% OpenType with CFF
is_font(<<0,1,0,0,?SKIP>>) -> true;  %% OpenType with 1.0
is_font(_) -> false.

find_cmap(Cmap, Bin) ->
    <<_:Cmap/binary, _:16, NumTables:?U16, Data/binary>> = Bin,
    Cmap + find_cmap1(NumTables, Data).

find_cmap1(0, _) -> throw(unsupported_format);
find_cmap1(_, <<?PLATFORM_ID_MICROSOFT:?U16,
		?MS_EID_UNICODE_BMP:?U16, Offset:?U32, ?SKIP>>) -> Offset;
find_cmap1(_, <<?PLATFORM_ID_MICROSOFT:?U16,
		?MS_EID_UNICODE_FULL:?U16, Offset:?U32, ?SKIP>>) -> Offset;
find_cmap1(NumTables, <<_:64, Next/binary>>) ->
    find_cmap1(NumTables-1, Next).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_table(<<_:32, NumTables:?U16, _SR:16, _ES:16, _RS:16, Tables/binary>>, Tag) ->
    find_table(NumTables, Tag, Tables).
find_table(0, _, _) -> false;
find_table(_, Tag, <<Tag:4/binary, _CheckSum:32, Offset:?U32, _Len:32, ?SKIP>>) ->
    Offset;
find_table(Num, Tag, <<_Tag:32, _CheckSum:32, _Offset:32, _Len:32, Next/binary>>) ->
    find_table(Num-1, Tag, Next).

num_glyphs(Bin) ->
    case find_table(Bin, <<"maxp">>) of
	false -> 16#ffff;
	Offset0 ->
	    Offset = Offset0+4,
	    <<_:Offset/binary, NG:?U16, ?SKIP>> = Bin,
	    NG
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Converts UnicodeCodePoint to Glyph index
%% Glyph 0 is the undefined glyph
-spec find_glyph_index(Font::ttf(), Char::integer()) -> Glyph::integer().
find_glyph_index(#ttf_info{data=Bin, index_map=IndexMap}, UnicodeCP) ->
    case Bin of
	%% Format0: Apple byte encoding
	<<_:IndexMap/binary, 0:?U16, Bytes:?U16, _:16, _:UnicodeCP/binary, Index:8, ?SKIP>>
	  when UnicodeCP < (Bytes-6) -> Index;
	<<_:IndexMap/binary, 0:?U16, ?SKIP>> -> 0;

	%% Format2: Mixed 8/16 bits mapping for Japanese, Chinese and Korean
	<<_:IndexMap/binary, 2:?U16, ?SKIP>> -> 0; %% TODO

	%% Format4: 16 bit mapping
	<<_:IndexMap/binary, 4:?U16, _Len:16, _Lan:16, Format4/binary>> ->
	    format_4_index(Format4, UnicodeCP);

	%% Format6: Dense 16 bit mapping
	<<_:IndexMap/binary, 6:?U16, _Len:16, _Lang:16,
	  First:?U16, Count:?U16, IndexArray/binary>> ->
	    case UnicodeCP >= First andalso UnicodeCP < (First+Count) of
		false -> 0;
		true  ->
		    Pos = (UnicodeCP - First)*2,
		    <<_:Pos/binary, Index:?U16, ?SKIP>> = IndexArray,
		    Index
	    end;
	%% Format8: Mixed 16/32 and pure 32 bit mappings
	<<_:IndexMap/binary, 8:16, ?SKIP>> -> 0;
	%% Format10: Mixed 16/32 and pure 32 bit mappings
	<<_:IndexMap/binary, 10:16, ?SKIP>> -> 0;
	%% Format12/13: Mixed 16/32 and pure 32 bit mappings
	<<_:IndexMap/binary, Format:?U16, _:16, _:32, _:32, Count:?U32, Groups/binary>>
	  when Format =:= 12; Format =:= 13 ->
	    format_32_search(0, Count, Groups, UnicodeCP, Format);
	%% Unsupported ( not specified )
	_ -> 0
    end.

format_4_index(_, Unicode) when Unicode >= 16#FFFF -> 0;
format_4_index(<<SegCountX2:?U16, SearchRange0:?U16, EntrySel:?U16,
		 RangeShift:?U16, Table/binary>>, Unicode) ->
    %% SegCount    = SegCountX2 div 2,
    SearchRange = SearchRange0 div 2,
    %% Binary Search
    <<EndCode:SegCountX2/binary, 0:16,
      StartCode:SegCountX2/binary,
      IdDelta:SegCountX2/binary,
      IdRangeOffset/binary  %% Also includes  GlyphIndexArray/binary
    >> = Table,
    %% they lie from endCount .. endCount + segCount
    %% but searchRange is the nearest power of two, so...
    Search = case EndCode of
		 <<_:RangeShift/binary, Search0:?U16, ?SKIP>>
		   when Unicode >= Search0 ->
		     RangeShift;
		 _ -> 0
	     end,
    Item = format_4_search(EntrySel, Search-2, SearchRange, EndCode, Unicode),
    case EndCode of
	<<_:Item/binary, Assert:16, ?SKIP>> ->
	    true = Unicode =< Assert;
	_ -> exit(assert)
    end,
    <<_:Item/binary, Start:?U16, ?SKIP>> = StartCode,
    %% <<_:Item/binary, End:?U16, ?SKIP>> = EndCode,
    <<_:Item/binary, Offset:?U16, ?SKIP>> = IdRangeOffset,

    if
	Unicode < Start ->
	    0;
	Offset =:= 0 ->
	    <<_:Item/binary, Index:?S16, ?SKIP>> = IdDelta,
	    Index + Unicode;
	true ->
	    Skip = Item + Offset + (Unicode - Start)*2,
	    <<_:Skip/binary, Index:?U16, ?SKIP>> = IdRangeOffset,
	    Index
    end.

format_4_search(EntrySel, Start, SearchRange, Bin, Unicode) when EntrySel > 0 ->
    Index = Start + SearchRange,
    case Bin of
	<<_:Index/binary, End:?U16, ?SKIP>> when Unicode > End ->
	    format_4_search(EntrySel-1, Start+SearchRange, SearchRange div 2, Bin, Unicode);
	_ ->
	    format_4_search(EntrySel-1, Start, SearchRange div 2, Bin, Unicode)
    end;
format_4_search(_, Search, _, _, _) ->
    Search+2.

format_32_search(Low, High, Groups, UnicodeCP, Format)
  when Low < High ->
    Mid = Low + ((High - Low) div 2),
    MidIndex = Mid*12,
    <<_:MidIndex/binary, Start:?U32, End:?U32, Glyph:?U32, ?SKIP>> = Groups,
    if
	UnicodeCP < Start ->
	    format_32_search(Low, Mid, Groups, UnicodeCP, Format);
	UnicodeCP > End ->
	    format_32_search(Mid+1, High, Groups, UnicodeCP, Format);
	Format =:= 12 ->
	    Glyph+UnicodeCP-Start;
	Format =:= 13 ->
	    Glyph
    end;
format_32_search(_, _, _, _, _) -> 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_glyf_offset(#ttf_info{num_glyphs=NumGlyphs}, Glyph)
  when Glyph >= NumGlyphs ->
    -1; %% Out of range
get_glyf_offset(#ttf_info{index_to_loc_format=0, data=Bin, loca=Loca, glyf=Glyf}, Glyph) ->
    Skip = Glyph*2,
    <<_:Loca/binary, _:Skip/binary, G1:?U16, G2:?U16, ?SKIP>> = Bin,
    case G1 == G2 of
	true -> -1;
	false -> Glyf + G1 * 2
    end;
get_glyf_offset(#ttf_info{index_to_loc_format=1, data=Bin, loca=Loca, glyf=Glyf}, Glyph) ->
    Skip = Glyph*4,
    <<_:Loca/binary, _:Skip/binary, G1:?U32, G2:?U32, ?SKIP>> = Bin,
    case G1 == G2 of
	true -> -1; %% Length is zero
	false -> Glyf + G1
    end;
get_glyf_offset(_, _) -> %% unknown glyph map format
    -1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% leftSideBearing is the offset from the current horizontal position
%% to the left edge of the character advanceWidth is the offset from
%% the current horizontal position to the next horizontal position
%% these are expressed in unscaled coordinates
-spec get_glyph_h_metrics(Font::ttf(), Glyph::integer()) ->
				 { Advance::integer(),
				   LeftSideBearing::integer()}.
get_glyph_h_metrics(#ttf_info{data=Bin, hhea=Hhea, hmtx=Hmtx}, Glyph) ->
    <<_:Hhea/binary, _:34/binary, LongHorMetrics:?U16, ?SKIP>> = Bin,
    case Glyph < LongHorMetrics of
	true ->
	    Skip = 4*Glyph,
	    <<_:Hmtx/binary, _:Skip/binary, Advance:?S16, LeftSideBearing:?S16, ?SKIP>> = Bin,
	    {Advance, LeftSideBearing};
	false ->
	    Skip1 = 4*(LongHorMetrics-1),
	    <<_:Hmtx/binary, _:Skip1/binary, Advance:?S16, ?SKIP>> = Bin,
	    Skip2 = 4*LongHorMetrics+2*(Glyph-LongHorMetrics),
	    <<_:Hmtx/binary, _:Skip2/binary, LeftSideBearing:?S16, ?SKIP>> = Bin,
	    {Advance, LeftSideBearing}
    end.

%% Computes a scale factor to produce a font whose EM size is mapped to
%% 'pixels' tall.
-spec scale_for_mapping_em_to_pixels(Font::ttf(), Size::float()) -> Scale::float().
scale_for_mapping_em_to_pixels(#ttf_info{data=Bin, head=Head}, Size) ->
    <<_:Head/binary, _:18/binary, UnitsPerEm:?U16, ?SKIP>> = Bin,
    Size / UnitsPerEm.


-spec get_glyph_box(Font::ttf(), Glyph::integer()) ->
			   {X0::integer(),Y0::integer(),
			    X1::integer(),Y1::integer()}.
get_glyph_box(TTF = #ttf_info{data=Bin}, Glyph) ->
    case get_glyf_offset(TTF, Glyph) of
	Offset when Offset > 0  ->
	    <<_:Offset/binary, _:16, X0:?S16, Y0:?S16, X1:?S16, Y1:?S16, ?SKIP>> = Bin,
	    {X0,Y0,X1,Y1};
	_ ->
	    {0,0,0,0}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_glyph_shape(Font::ttf(), Glyph::integer()) -> Vertices::vertex().
get_glyph_shape(TTF, Glyph) ->
    get_glyph_shape_impl(TTF, get_glyf_offset(TTF, Glyph)).

get_glyph_shape_impl(_TTF, Offset)
  when Offset < 0 -> [];
get_glyph_shape_impl(TTF = #ttf_info{data=Bin}, Offset) ->
    <<_:Offset/binary, NumberOfContours:?S16,
      _XMin:16, _YMin:16, _XMax:16, _YMax:16,
      GlyphDesc/binary>> = Bin,
    if NumberOfContours > 0 ->
	    %% Single Glyph
	    Skip = NumberOfContours*2 - 2,
	    <<_:Skip/binary, Last:?U16, InsLen:?U16, Instr/binary>> = GlyphDesc,
	    N = 1 + Last,
	    <<_:InsLen/binary, FlagsBin/binary>> = Instr,
	    %%io:format("Conts ~p ~p ~p~n",[NumberOfContours, InsLen, N]),
	    {Flags, XCoordsBin} = parse_flags(N, 0, FlagsBin, []),
	    {XCs, YCoordsBin} = parse_coords(Flags, XCoordsBin, 0, 2, []),
	    {YCs, _} = parse_coords(Flags, YCoordsBin, 0, 4, []),
	    N = length(Flags),
	    setup_vertices(Flags, XCs, YCs, GlyphDesc);
       NumberOfContours =:= -1 ->
	    %% Several Glyphs (Compund shapes)
	    get_glyph_shapes(GlyphDesc, TTF, []);
       NumberOfContours < -1 ->
	    exit(bad_ttf);
       NumberOfContours =:= 0 ->
	    []
    end.

parse_flags(N, 0, <<Flag:8, Rest/binary>>, Flags)
  when N > 0 ->
    case (Flag band 8) > 1 of
	false ->
	    parse_flags(N-1, 0, Rest, [Flag|Flags]);
        true ->
	    <<Repeat:8, Next/binary>> = Rest,
	    parse_flags(N-1, Repeat, Next, [Flag|Flags])
    end;
parse_flags(N, R, Rest, Flags = [Prev|_])
  when N > 0 ->
    parse_flags(N-1, R-1, Rest, [Prev|Flags]);
parse_flags(0, 0, Rest, Flags) -> {lists:reverse(Flags), Rest}.

%% repeat(0, _, Flags) -> Flags;
%% repeat(N, Flag, Flags) -> repeat(N-1, Flag, [Flag|Flags]).

parse_coords([Flag|Flags], <<DX:8, Coords/binary>>, X0, Mask, Xs)
  when (Flag band Mask) > 1, (Flag band (Mask*8)) > 1 ->
    X = X0+DX,
    parse_coords(Flags, Coords, X, Mask, [X|Xs]);
parse_coords([Flag|Flags], <<DX:8, Coords/binary>>, X0, Mask, Xs)
  when (Flag band Mask) > 1 ->
    X = X0-DX,
    parse_coords(Flags, Coords, X, Mask, [X|Xs]);
parse_coords([Flag|Flags], Coords, X, Mask, Xs)
  when (Flag band (Mask*8)) > 1 ->
    parse_coords(Flags, Coords, X, Mask, [X|Xs]);
parse_coords([_|Flags], <<DX:?S16, Coords/binary>>, X0, Mask, Xs) ->
    X = X0 + DX,
    parse_coords(Flags, Coords, X, Mask, [X|Xs]);
parse_coords([], Rest, _, _, Xs) ->
    {lists:reverse(Xs), Rest}.

setup_vertices(Flags, XCs, YCs, GlyphDesc) ->
    setup_vertices(Flags, XCs, YCs, GlyphDesc, 0, -1, {0,0}, false,false, []).

setup_vertices([Flag|Fs0], [X|XCs0], [Y|YCs0], GD, StartC, Index,
	       S0, WasOff, StartOff0, Vs0)
  when StartC < 2 ->
    Vs1 = case StartC of
	      0 -> Vs0; %% First
	      1 -> close_shape(Vs0, S0, WasOff, StartOff0)
	  end,
    %% Start new one
    <<Next0:?U16, NextGD/binary>> = GD,
    Next = Next0-Index,
    case (Flag band 1) =:= 0 of
	true  ->
	    StartOff = {X,Y}, %% Save for warparound
	    [FN|Fs1]  = Fs0,
	    [XN|Xcs1] = XCs0,
	    [YN|Ycs1] = YCs0,
	    {S,Skip,Fs,XCs,YCs} =
		case ((FN band 1) =:= 0) of
		    true -> %% Next is also off
			{{(X+XN) div 2, (Y+YN) div 2},0,
			 Fs0, XCs0, YCs0};
		    false ->
			{{XN, YN},1,Fs1,Xcs1,Ycs1}
		end,
	    %%io:format("SOff ~p ~p ~p~n",[(Flag band 1) =:= 0, S, Next]),
	    Vs = set_vertex(Vs1, move, S, {0,0}),
	    setup_vertices(Fs,XCs,YCs,NextGD,Next-Skip,Next0,S,false,StartOff,Vs);
	false ->
	    S = {X,Y},
	    %%io:format("Start ~p ~p ~p~n",[(Flag band 1) =:= 0, S, Next]),
	    Vs = set_vertex(Vs1, move, S, {0,0}),
	    setup_vertices(Fs0,XCs0,YCs0,NextGD,Next,Next0,S,false,false,Vs)
    end;
setup_vertices([Flag|Fs], [X|XCs], [Y|YCs], GD, Next,Index,S,WasOff,StartOff,Vs0) ->
    %%io:format("~p ~p~n",[(Flag band 1) =:= 0, WasOff /= false]),
    case {(Flag band 1) =:= 0, WasOff} of
	{true, {Cx,Cy}} ->
	    %%  two off-curve control points in a row means interpolate an on-curve midpoint
	    Int = {(X+Cx) div 2, (Y+Cy) div 2},
	    Vs = set_vertex(Vs0, curve, Int, WasOff),
	    setup_vertices(Fs,XCs,YCs, GD, Next-1,Index,S,{X,Y}, StartOff, Vs);
	{true, false} ->
	    setup_vertices(Fs,XCs,YCs, GD, Next-1,Index,S,{X,Y}, StartOff, Vs0);
	{false,false} ->
	    Vs = set_vertex(Vs0, line, {X,Y}, {0,0}),
	    setup_vertices(Fs,XCs,YCs, GD, Next-1,Index,S,false, StartOff, Vs);
	{false,C} ->
	    Vs = set_vertex(Vs0, curve, {X,Y}, C),
	    setup_vertices(Fs,XCs,YCs, GD, Next-1,Index,S,false, StartOff, Vs)
    end;
setup_vertices([], [], [], _, _Next, _, S, WasOff, StartOff, Vs) ->
    lists:reverse(close_shape(Vs, S, WasOff, StartOff)).

close_shape(Vs0, S={SX,SY}, C={CX,CY}, SC={_SCX,_SCY}) ->
    Vs1 = set_vertex(Vs0, curve, {(SX+CX) div 2, (SY+CY) div 2}, C),
    set_vertex(Vs1, curve, S, SC);
close_shape(Vs, S, false, SC={_SCX,_SCY}) ->
    set_vertex(Vs, curve, S, SC);
close_shape(Vs, S, C={_CX,_CY}, false) ->
    set_vertex(Vs, curve, S, C);
close_shape(Vs, S, false, false) ->
    set_vertex(Vs, line, S, {0,0}).

set_vertex(Vs, Mode, Pos, C) ->
    %%io:format("V ~p ~p ~p~n",[Pos, C, Mode]),
    [#vertex{type=Mode, pos=Pos, c=C}|Vs].

get_glyph_shapes(<<Flags:?S16, GidX:?S16, GlyphDesc0/binary>>, Font, Vs0) ->
    {ScaleInfo,GlyphDesc} = find_trans_scales(Flags, GlyphDesc0),
    Vs1 = get_glyph_shape(Font, GidX),
    Vs = scale_vertices(Vs1, ScaleInfo, Vs0),
    case (Flags band (1 bsl 5)) > 1 of
	true -> %% More Compontents
	    get_glyph_shapes(GlyphDesc, Font, Vs);
	false ->
	    lists:reverse(Vs)
    end.

find_trans_scales(Flags,
		  <<Mtx4:?S16, Mtx5:?S16, GlyphDesc/binary>>)
  when (Flags band 3) > 2 ->
    find_trans_scales(Flags, Mtx4, Mtx5, GlyphDesc);
find_trans_scales(Flags, <<Mtx4:8, Mtx5:8, GlyphDesc/binary>>)
  when (Flags band 2) > 1 ->
    find_trans_scales(Flags, Mtx4, Mtx5, GlyphDesc).
%% @TODO handle matching point
%%find_trans_scales(Flags, GlyphDesc0) ->

find_trans_scales(Flags, Mtx4, Mtx5, <<Mtx0:?S16, GlyphDesc/binary>>)
  when (Flags band (1 bsl 3)) > 1 ->
    %% We have a scale
    S = 1 / 16384,
    {calc_trans_scales(Mtx0*S, 0, 0, Mtx0*S, Mtx4, Mtx5),GlyphDesc};
find_trans_scales(Flags, Mtx4, Mtx5, <<Mtx0:?S16, Mtx3:?S16, GlyphDesc/binary>>)
  when (Flags band (1 bsl 6)) > 1 ->
    %% We have a X and Y scale
    S = 1 / 16384,
    {calc_trans_scales(Mtx0*S, 0, 0, Mtx3*S, Mtx4, Mtx5), GlyphDesc};
find_trans_scales(Flags, Mtx4, Mtx5,
		  <<Mtx0:?S16, Mtx1:?S16,
		    Mtx2:?S16, Mtx3:?S16, GlyphDesc/binary>>)
  when (Flags band (1 bsl 7)) > 1 ->
    %% We have a two by two
    S = 1 / 16384,
    {calc_trans_scales(Mtx0*S, Mtx1*S, Mtx2*S, Mtx3*S, Mtx4, Mtx5), GlyphDesc};
find_trans_scales(_, Mtx4, Mtx5, GlyphDesc) ->
    {calc_trans_scales(1.0, 0.0, 0.0, 1.0, Mtx4, Mtx5), GlyphDesc}.

calc_trans_scales(Mtx0, Mtx1, Mtx2, Mtx3, Mtx4, Mtx5) ->
    {math:sqrt(square(Mtx0)+square(Mtx1)),
     math:sqrt(square(Mtx2)+square(Mtx3)), Mtx0, Mtx1, Mtx2, Mtx3, Mtx4, Mtx5}.

scale_vertices([#vertex{pos={X,Y}, c={CX,CY}, type=Type}|Vs],
	       SI={M,N, Mtx0, Mtx1, Mtx2, Mtx3, Mtx4, Mtx5}, Acc) ->
    V = #vertex{type=Type,
		pos = {round(M*(Mtx0*X+Mtx2*Y+Mtx4)),
		       round(N*(Mtx1*X+Mtx3*Y+Mtx5))},
		c   = {round(M*(Mtx0*CX+Mtx2*CY+Mtx4)),
		       round(N*(Mtx1*CX+Mtx3*CY+Mtx5))}},
    scale_vertices(Vs, SI, [V|Acc]);
scale_vertices([], _, Acc) -> Acc.

square(X) -> X*X.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
