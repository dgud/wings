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
%%  the standard, and ttfc and otc files. /Dan
%%

%% For TrueType format, see http://www.microsoft.com/typography/otspec/

-module(wpc_tt).
-export([init/0,menu/2,command/2,
         init_font/2, sysfontdirs/0, process_ttfs/1, trygen/3, trygen/5, find_font_info/1 % debugging
        ]). % for ai

-import(lists, [reverse/1,sort/2,keysearch/3,duplicate/2,nthtail/2,
		mapfoldl/3,foldl/3,sublist/3,map/2,last/1,seq/2,seq/3,
		flatten/1,sum/1,append/1]).

-include_lib("wings/src/wings.hrl").
-include_lib("wings/e3d/e3d.hrl").

-record(ttf_info,
	{num_glyphs,     %% Number of Glyphs

	 %% Offsets to table locations
	 loca, glyf, head, hhea, hmtx, kern, name, os2,
         cff,           %% undefined or initatied CFF info
	 index_map,     %% A Cmap mapping for out  chosen character encoding
	 index_to_loc_format, %% Format needed to map from glyph index to glyph
         file,          %% Filename
         collection=0,  %% Collection number
	 data           %% The binary file
	}).

-record(polyarea,
	{boundary,			%%list of cedges (CCW oriented, closed)
	 islands=[]}).			%%list of lists of cedges (CW, closed)

-record(vertex, {pos, c, c1, type}).

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

-define(DBG(F,A), ok).
%-define(DBG(F,A), io:format("~w:~w: "++ F, [?MODULE,?LINE] ++ A)).

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
		{?__(5,"Number of edge bisections"),
                 {slider,{text,Bisect,[{key,{wpc_tt,bisections}},{range,{0,4}}]}}},
		{?__(3,"TrueType font"),
                 {fontpicker,FontInfo,[{key,{wpc_tt,font}}]}}]},
	    wings_shapes:transform_obj_dlg()
	],[{margin,false}]
	 }],
    Fun = fun({dialog_preview,[T,N,{_,Ctrl},RX,RY,RZ,MX,MY,MZ,Grnd]=_Res}) ->
                  {NewFontI, FPath} = find_font_file(GbtFonts,Ctrl),
                  Size = maps:get(size, NewFontI),
                  {preview,{shape,{text,[T,N,{fontdir,FPath},
                                         Size,RX,RY,RZ,MX,MY,MZ,Grnd]}},St};
             (cancel) ->
                  St;
             ([T,N,{_,WxFont},RX,RY,RZ,MX,MY,MZ,Grnd]=_Res) when is_tuple(WxFont) ->
                  {NewFontI, FPath} = find_font_file(GbtFonts,WxFont),
                  Size = maps:get(size, NewFontI),
                  case FPath of
                      {_, undefined} ->
                          St;
                      _ ->
                          wpa:pref_set(wpc_tt, fontname, NewFontI),
                          wpa:pref_set(wpc_tt, text, element(2,T)),
                          wpa:pref_set(wpc_tt, bisections, element(2,N)),
                          {commit,{shape,{text,[T,N,{fontdir,FPath},
                                                Size,RX,RY,RZ,MX,MY,MZ,Grnd]}},St}
                  end
          end,
    wings_dialog:dialog(Ask,?__(1,"Create Text"), {preview, Dlg}, Fun);

make_text([{_,T},{_,N},{_,FontFile}, Size|Transf], _) ->
    %% Assuming 10 ppi is 2 w.u. which the other primitives are
    gen(FontFile, T, N, Size*0.25, Transf).

help_button() ->
    Title = ?__(1,"Select font"),
    TextFun = fun () -> help() end,
    {help,Title,TextFun}.

help() ->
    [?__(1,"Only TrueType (OpenType) fonts can be used \n"
         "and they must be installed in the standard operating system\n"
         "directories for fonts.")].

gen(_FontFile, "", _Nsubsteps, _, _Transf) ->
    keep;
gen({FName,{{error, Reason, _}, _Idx}}, _, _, _, _) ->
    Msg = ?__(1,"Text: ") ++ FName ++ " " ++ format_error(Reason),
    wpa:error_msg(Msg);
gen({FName,undefined}, _, _, _, _) ->
    Msg = ?__(2,"Text: Failed to locate the TTF file or unknown format: ") ++ FName,
    wpa:error_msg(Msg);
gen({_FName, {File, Idx}}, Text, Nsubsteps, Size, Transf) ->
    try trygen(File, Text, Idx, Nsubsteps, Size) of
	{new_shape,Name,Fs0,Vs0,He} ->
	    Vs = wings_shapes:transform_obj(Transf, Vs0),
	    Fs = [#e3d_face{vs=Vsidx} || Vsidx <- Fs0],
	    Mesh = #e3d_mesh{type=polygon, vs=Vs, fs=Fs, he=He},
	    {new_shape, Name, #e3d_object{obj=Mesh}, []};
        keep ->
            keep
    catch
        throw:{error, _What, Msg}:_St ->
            %% ?DBG("error: ~p ~p~n ~P~n", [_What, Msg, _St, 40]),
            wpa:error_msg(Msg);
        _:X:ST ->
	    io:format(?__(5,"caught error: ") ++"~P~nST:~p", [X, 40,ST]),
	    wpa:error_msg(?__(6,"Text failed: internal error"))
    end.

process_ttfs(Dirs) ->
    case ets:info(?MODULE) of
        undefined ->
            Tab = ets:new(?MODULE, [named_table, public]),
            Add = fun(FileName, _Acc) ->
                          Store = fun(FontInfo0, Idx) ->
                                          {Key,_} = KV =
                                              case FontInfo0 of
                                                  {error, Reason, FI} ->
                                                      {FI, {{error, Reason, FileName}, Idx}};
                                                  FI ->
                                                      {FI, {FileName,Idx}}
                                              end,
                                          case ets:lookup(Tab, Key) of
                                              [] -> ok;
                                              [_Old] ->
                                                  %% ?DBG("Overwrite Font Info:~nOLD: ~p~nNEW: ~p~n",
                                                  %%      [_Old,{FontInfo,{FileName,Idx}}]),
                                                  ok
                                          end,
                                          true = ets:insert(Tab, KV),
                                          Idx+1
                                  end,
                          try find_font_info(FileName) of
                              List ->
                                  lists:foldl(Store, 0, List),
                                  ok
                          catch _:_What:_St ->
                                  ?DBG("Fail: ~p : ~P~n ~P~n",[FileName, _What, 20, _St, 20]),
                                  ok
                          end
                  end,
            Filter = ".ttf|.TTF|.ttc|.TTC|.otf|.OTF",
            lists:foldl(fun(Dir, Tree) ->
                                filelib:fold_files(Dir, Filter, true, Add, Tree)
                        end, ok, Dirs),
            Tab;
        [_|_] ->
            ?MODULE
    end.

trygen(File, Text, SubDiv) ->
    trygen(File, Text, 0, SubDiv, 2).
trygen(File, Text, Idx, Nsubsteps, Size) ->
    TTF = init_font(File, Idx),
    ?DBG("~P~n",[TTF, 20]),
    Pa0 = get_polyareas(Text, TTF, Nsubsteps, Size),
    {Vs0,Fs,He} = polyareas_to_faces(Pa0),
    case Vs0 of
        [_|_] ->
            {CX,_CY,CZ} = e3d_vec:average(tuple_to_list(e3d_bv:box(Vs0))),
            Vs = [{X-CX,Y,Z-CZ} || {X,Y,Z} <- Vs0],
            {new_shape,"text: " ++ Text,Fs,Vs,He};
        [] ->
            keep
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
    System = case winregval("", "SystemRoot") of
                 none -> Def;
                 Val -> Val
             end,
    UserInstalled = filename:join(filename:basedir(user_data, "Microsoft"), "Windows"),
    [filename:join(System, "Fonts"), filename:join(UserInstalled, "Fonts")];
sysfontdirs({unix,darwin}) ->
    Home = os:getenv("HOME"),
    ["/Library/Fonts", "/System/Library/Fonts/", filename:join(Home, "Library/Fonts")];
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

%% is_ttfc(<<"ttcf", ?SKIP>>) -> true;
%% is_ttfc(_) -> false.

get_polyareas(Text, Font, Nsubsteps, Size) ->
    Scale = scale_for_mapping_em_to_pixels(Font, Size),
    Area = fun(CodePoint, {X,Acc}) ->
                   Glyph = find_glyph_index(Font, CodePoint),
                   {Advance, _} = get_glyph_h_metrics(Font, Glyph),
                   %% ?DBG("Char: ~c Glyph: ~w Advance: ~p*~p=~p~n",
                   %%      [CodePoint, Glyph, Advance, Scale, Advance*Scale]),
                   Areas = get_polyarea(Glyph, X, Scale, Nsubsteps, Font),
                   %% ?DBG(" ~150.p~n", [Areas]),
                   {X+Advance, Areas ++ Acc}
           end,
    {_, Pas} = lists:foldl(Area, {0, []}, Text),
    Pas.

get_polyarea(Glyph, X, Scale, Nsubsteps, Font) ->
    GlyphVs = get_glyph_shape(Font, Glyph),
    VsCont = verts_to_point_lists(GlyphVs, Scale, Nsubsteps),
    {_X0,_Y0,_X1,_Y1} = bb_box(VsCont),
    %% io:format("~p:~p: ~p ~p~n",[?MODULE,?LINE, Scale, get_glyph_box(Font, Glyph)]),

    Make = fun(Vs) -> make_edges(Vs, {Scale,Scale}, {X*Scale, 0}, []) end,
    Edges0 = lists:map(Make, VsCont),
    Edges = [Edge || [_|_] = Edge <- Edges0],  %% Filter out empty lists
    %% io:format("~p:~p: Edges: ~n  ~w~n",[?MODULE,?LINE, Edges]),
    findpolyareas(Edges).

verts_to_point_lists(Vs, Scale, SubDiv) ->
    F = {8.0/(Scale*math:pow(8,SubDiv)), SubDiv},
    lists:reverse(verts_to_points(Vs, {0.0,0.0}, F, [], [])).

verts_to_points([#vertex{type=move,pos=Point}|Vs], _, F, Cont, All) ->
    verts_to_points(Vs, Point, F, [Point], add_contour(Cont, All));
verts_to_points([#vertex{type=line,pos=Point}|Vs], _, F, Cont, All) ->
    verts_to_points(Vs, Point, F, [Point|Cont], All);
verts_to_points([#vertex{type=curve,pos=VP={PX,PY},c={CX,CY}}|Vs],
                {X,Y}, {Limit,Level}=F, Cont0, All) ->
    Cont = tesselate_curve(X,Y, CX,CY, PX,PY, Limit, Level, Cont0),
    verts_to_points(Vs, VP, F, Cont, All);
verts_to_points([#vertex{type=cubic,pos=VP={PX,PY},c={CX,CY}, c1={CX1,CY1}}|Vs],
                {X,Y}, {Limit,Level}=F, Cont0, All) ->
    Cont = tesselate_cubic(X,Y, CX,CY, CX1,CY1, PX,PY, Limit, Level, Cont0),
    verts_to_points(Vs, VP, F, Cont, All);
verts_to_points([], _, _, Cont, All) ->
    add_contour(Cont, All).

add_contour([], All) -> All;
add_contour(Cont, All) ->
    [lists:reverse(Cont)|All].

tesselate_curve(X0,Y0, X1,Y1, X2,Y2, Limit, Level, Cont0) when Level >= 0 ->
    Mx = (X0 + 2*X1 + X2)/4.0,
    My = (Y0 + 2*Y1 + Y2)/4.0,
    %% Versus Directly Drawn Line
    Dx = (X0+X2)/2.0 - Mx,
    Dy = (Y0+Y2)/2.0 - My,
    %% io:format(" ~p,~p ~p,~p => ~p > ~p~n",[X0,Y0,X1,Y1,Dx*Dx+Dy*Dy,Limit]),
    if (Dx*Dx+Dy*Dy) > Limit ->
	    Cont1 = tesselate_curve(X0,Y0, (X0+X1)/2.0,(Y0+Y1)/2.0, Mx,My, Limit, Level-1, Cont0),
	    tesselate_curve(Mx,My, (X1+X2)/2.0,(Y1+Y2)/2.0, X2,Y2, Limit, Level-1, Cont1);
       true ->
	    [{X2,Y2}|Cont0]
    end;
tesselate_curve(_X0,_Y0, _X1,_Y1, X2,Y2, _F, _Level, Cont) ->
    [{X2,Y2}|Cont].

tesselate_cubic(X0,Y0, X1,Y1, X2,Y2, X3,Y3, Limit, Level, Cont0) when Level >= 0 ->
    Dx0 = X1-X0, Dy0 = Y1-Y0,
    Dx1 = X2-X1, Dy1 = Y2-Y1,
    Dx2 = X3-X2, Dy2 = Y3-Y2,
    Dx  = X3-X0, Dy  = Y3-Y0,

    LL = math:sqrt(Dx0*Dx0+Dy0*Dy0)+math:sqrt(Dx1*Dx1+Dy1*Dy1)+math:sqrt(Dx2*Dx2+Dy2*Dy2),
    SL = math:sqrt(Dx*Dx+Dy*Dy),

    if (LL*LL-SL*SL) > Limit ->
            X01 = (X0+X1)/2,  Y01 = (Y0+Y1)/2,
            X12 = (X1+X2)/2,  Y12 = (Y1+Y2)/2,
            X23 = (X2+X3)/2,  Y23 = (Y2+Y3)/2,

            Xa  = (X01+X12)/2, Ya = (Y01+Y12)/2,
            Xb  = (X12+X23)/2, Yb = (Y12+Y23)/2,

            Mx  = (Xa+Xb)/2,   My = (Ya+Yb)/2,

	    Cont1 = tesselate_cubic(X0,Y0, X01,Y01, Xa,Ya, Mx,My, Limit, Level-1, Cont0),
	    tesselate_cubic(Mx,My, Xb,Yb, X23,Y23, X3,Y3, Limit, Level-1, Cont1);
       true ->
	    [{X3,Y3}|Cont0]
    end;
tesselate_cubic(_X0,_Y0, _X1,_Y1, _X2,_Y2, X3,Y3, _F, _Level, Cont) ->
    [{X3,Y3}|Cont].


bb_box(ListOfLists) ->
    MinMax = fun({X,Y}, {MinX,MinY,MaxX,MaxY}) ->
                     {min(X,MinX), min(Y,MinY),
                      max(X,MaxX), max(X,MaxY)}
             end,
    lists:foldl(fun(List, Acc) -> lists:foldl(MinMax, Acc, List) end,
                {0.0,0.0, 0.0,0.0}, ListOfLists).

make_edges([{JX,JY}|Rest=[{KX,KY}|_]], Scale = {ScX, ScY}, Shift = {ShX,ShY}, Eds) ->
    Edge = {{JX * ScX + ShX, JY * ScY + ShY},
            {KX * ScX + ShX, KY * ScY + ShY}},
    case Edge of
        {V,V} ->  %% Remove zero size edges
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

%% Return true if there is no unassigned J <= second arg, J /= I,
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
    0.5 * foldl(fun ({{X1,Y1},{X2,Y2}},A) ->
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
revcedge({Vs,Ve}) -> {Ve,Vs}.

%% classify vertices of contour B with respect to contour A.
%% return {# inside A, # on A}.
classifyverts(A,B) ->
    foldl(fun({Vb,_},Acc) -> cfv(A,Vb,Acc) end, {0,0}, B).


%% Decide whether vertex P is inside or on (as a vertex) contour A,
%% and return modified pair.  Assumes A is CCW oriented.
%% CF Eric Haines ptinpoly.c in Graphics Gems IV
cfv(A,P,{Inside,On}) ->
    {Va0, _} = last(A),
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
vinside([{{X1,Y1}=V1,_}|Arest], {X0,Y0}, P={Xp,Yp}, Inside, Yflag0) ->
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

cel2vec(Cel, Z) -> map(fun({{X,Y},_}) -> {X,Y,Z} end, Cel).

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
encoding(5, unicode) -> {unicode_nyi, format_14};

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

-spec init_font(FileName, Index) -> ttf() when
      FileName :: list(),
      Index :: integer().
init_font(Filename, Index) ->
    case file:read_file(Filename) of
	{ok, Bin} -> init_font_1(Filename, Bin, Index);
	{error,Error} -> throw({error, Error, "Couldn't open file" ++ Filename})
    end.

init_font_1(Filename, Bin0, Index) ->
    Bin  = get_font_from_offset(Bin0, Index),
    is_font(Bin) orelse throw({error, bad_ttf_file}),
    Tabs = find_tables(Bin),
    Name = case maps:get(<<"name">>, Tabs, undefined) of
               undefined -> throw({error, bad_ttf_file});
               NameData -> NameData
           end,
    Os2  = maps:get(<<"OS/2">>, Tabs, undefined),
    try
        CMap = maps:get(<<"cmap">>, Tabs),
        %% Either loca and glyf
        Loca = maps:get(<<"loca">>, Tabs, undefined),
        Glyf = maps:get(<<"glyf">>, Tabs, undefined),
        %% or CFF is needed
        Cff  = maps:get(<<"CFF ">>, Tabs, undefined),
        Head = maps:get(<<"head">>, Tabs),
        Hhea = maps:get(<<"hhea">>, Tabs),
        Hmtx = maps:get(<<"hmtx">>, Tabs),
        Kern = maps:get(<<"kern">>, Tabs, undefined),
        NumGlyphs = num_glyphs(maps:get(<<"maxp">>, Tabs, undefined), Bin0),
        IndexMap  = find_index_map(CMap, Bin0),
        CffMap = pp_cff(Cff, Bin0, Filename),
        (Loca == undefined orelse Glyf == undefined)
            andalso CffMap == undefined
            andalso throw({error, no_glyf_info}),
        Skip = Head+50,
        <<_:Skip/binary, LocFormat:?U16, ?SKIP>> = Bin0,
        #ttf_info{data = Bin0, file = Filename, collection = Index,
                  name = Name, os2 = Os2,
                  num_glyphs = NumGlyphs,
                  loca = Loca, glyf = Glyf,
                  cff = CffMap,
                  head = Head, hhea = Hhea,
                  hmtx = Hmtx, kern = Kern,
                  index_map = IndexMap,
                  index_to_loc_format = LocFormat
                 }
    catch error:_Err:_ST ->
            %% We create a bad tff_info here to give other user error messages
            %% than file not found
            io:format("Parse error: ~p~n~P:~n  ~P~n",[Filename, _Err,30,_ST, 100]),
            throw({error, parse_error,
                   #ttf_info{name=Name, os2=Os2, data=Bin0,
                             file = {error, parse_error, Filename}}});
          throw:{error,_Err} ->
            throw({error,_Err,
                   #ttf_info{name=Name, os2=Os2, data=Bin0,
                             file = {error, _Err, Filename}}})
    end.

format_error(Error) ->
    io:format("TFF error: ~p~n", [Error]),
    ?__(1,"Unsupported ttf format").

find_font_info(File) ->
    {ok,Filecontents} = file:read_file(File),
    find_font_info(Filecontents, File).

find_font_info(<<"ttcf", 0,_V,0,0, N:32, ?SKIP >> = Bin, File) ->
    %% ?DBG("Version ~w: Size ~w~n",[V,N]),
    Info = fun(Idx, Acc) ->
                   try
                       Font = init_font_1(File, Bin, Idx),
                       [find_font_info_1(Font)|Acc]
                   catch throw:_Reason ->
                           ?DBG("~s:~w: ~p~n", [File, Idx, _Reason]),
                           Acc
                   end
           end,
    lists:foldl(Info, [], lists:seq(0,N-1));
find_font_info(Bin, File) ->
    try init_font_1(File, Bin,0) of
        Font ->
            [find_font_info_1(Font)]
    catch throw:{error, Reason, #ttf_info{}=Font} ->
            ?DBG("~s: ~p~n", [File, Reason]),
            [{error, Reason, find_font_info_1(Font)}]
    end.

find_font_info_1(#ttf_info{file=_File, collection=_Coll} = TTF) ->
    FontInfo = font_info(TTF),
    Family = proplists:get_value(family, FontInfo, undefined),
    PrefFamily = proplists:get_value(preferred_family, FontInfo, undefined),
    {Style, Weight} = font_styles(TTF),
    %% ?DBG("~p (~w) ~p ~s ~s~n  ~0.p~n~n", [_File, _Coll, Family, Style, Weight, FontInfo]),
    %% io:format("File: ~p ~p ~p ~p ~p~n", [_File,Family, PrefFamily, Style, Weight]),
    case PrefFamily of
        undefined -> {Family, Family,Style,Weight};
        _ -> {Family, PrefFamily, Style, Weight}
    end.

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

font_styles(#ttf_info{data=Bin, os2=TabOffset}) when is_integer(TabOffset) ->
    <<_:TabOffset/binary,_Ver:16,_:16,Weight:?U16,_Pad:26/binary,
      _Panose:10/binary,_ChrRng:16/binary,_VenId:4/binary,
      FsSel:16,_T1/binary>> = Bin,
    FStyle = if (FsSel band ?fsITALIC) =:= ?fsITALIC -> italic;
                true -> normal
             end,
    FWeight = if
                  Weight < 150 -> light;  %% thin
                  Weight < 250 -> light;  %% extra-light
                  Weight < 350 -> light;
                  Weight < 450 -> normal;
                  Weight < 550 -> normal; %% medium
                  Weight < 650 -> bold; %% semi-bold
                  Weight < 750 -> bold;
                  Weight < 850 -> bold; %% extra-bold
                  true -> bold %% black
              end,
    {FStyle, FWeight};
font_styles(_) ->
    {normal, normal}.

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
font_info(#ttf_info{data=Bin, name=Name}, Id, Platform, Encoding, Language) ->
    <<_:Name/binary, _V:16, Count:?U16, StringOffset:?U16, FI/binary>> = Bin,
    <<_:Name/binary, _:StringOffset/binary, Strings/binary>> = Bin,
    get_font_info(Count, FI, Strings, Id, Platform, Encoding, Language).

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
	    [{info(NId), string(String, Encoding)}|
	     get_font_info(N-1, Rest, Strings, WIds, WPlatform, WEnc, WLang)];
	_ ->
	    get_font_info(N-1, Rest, Strings, WIds, WPlatform, WEnc, WLang)
    end.

get_font_from_offset(<<"ttcf", 0,_V,0,0, N:32, Rest0/binary >> = Bin, Index)
  when N > Index ->
    Pos = Index*4,
    <<_:Pos/binary, Offset:32, ?SKIP>> = Rest0,
    <<_:Offset/binary, TTF/binary>> = Bin,
    TTF;
get_font_from_offset(Bin, 0) ->
    Bin.

find_font_file(Table, WxFont) ->
    FontInfo = wings_text:get_font_info(WxFont),
    try
        #{face:=FName, style:=FStyle, weight:=FWeight} = FontInfo,
        Alternatives = find_font_file_0(Table, FName, true),
        ?DBG("~p => ~p~n", [FontInfo, Alternatives]),
        File = select_fontfile(Alternatives, FStyle, FWeight),
        ?DBG("FontFile: ~p~n", [File]),
        {FontInfo, {FName, File}}
    catch _:Er:St ->
            io:format("~p: ~p~n",[Er,St]),
            {FontInfo, undefined}
    end.

find_font_file_0(Tab, [$@|FName], TryWin) ->
    %% Some fonts in windows start with a '@' do know why
    %% but they can't be found so remove '@'
    find_font_file_0(Tab, FName, TryWin);
find_font_file_0(Tab, FName, TryWin) ->
    case ets:match_object(Tab, {{FName,'_', '_', '_'}, '_'}) of
        [] ->
            case ets:match_object(Tab, {{'_', FName, '_', '_'},'_'}) of
                [] when TryWin ->
                    find_font_file_1(Tab, FName);
                List ->
                    List
            end;
        List ->
            List
    end.

find_font_file_1(Table,FName) ->
    case winregval("FontSubstitutes",FName) of
        none -> [];
        FSName -> find_font_file_0(Table, FSName, false)
    end.

select_fontfile(Alts0, Style, Weight) ->
    Alts = case [FI || {{_,_,S,_}, _} = FI <- Alts0, S =:= Style] of
               [] ->
                   case [FI || {{_,_,normal,_}, _} = FI <- Alts0] of
                       [] -> Alts0;
                       As -> As
                   end;
               As -> As
           end,
    select_fontfile_1(Alts, Weight).

select_fontfile_1(Alts0, Weight) ->
    Alts = case [FI || {{_,_,_, W}, _} = FI <- Alts0, W =:= Weight] of
               [] ->
                   case [FI || {{_,_,_,normal}, _} = FI <- Alts0] of
                       [] -> Alts0;
                       As -> As
                   end;
               As -> As
           end,
    select_fontfile_2(Alts).

select_fontfile_2([]) ->
    undefined;
select_fontfile_2([{_, File}|R] = _Alts) ->
    R =/= [] andalso ?DBG("Selecting hd of ~p~n",[_Alts]),
    File.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_tables(<<_:32, NumTables:?U16, _SR:16, _ES:16, _RS:16, Tables/binary>>) ->
    find_table(NumTables, Tables, []).

find_table(0, _, Tabs) -> maps:from_list(Tabs);
find_table(Num, <<Tag:4/binary, _CheckSum:32, Offset:32, _Len:32, Next/binary>>, Tabs) ->
    find_table(Num-1, Next, [{Tag, Offset}|Tabs]).

num_glyphs(undefined, _Bin) ->
    16#ffff;
num_glyphs(Offset0, Bin) ->
    Offset = Offset0+4,
    <<_:Offset/binary, NG:?U16, ?SKIP>> = Bin,
    NG.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_index_map(Cmap, Bin) ->
    <<_:Cmap/binary, _:16, NumTables:?U16, Data/binary>> = Bin,
    case find_index_map1(NumTables, Data, []) of
        [] -> throw({error, supported_index_map_not_found});
        Alternatives ->
            [{_, Offset}|_] = lists:sort(Alternatives),
            %% ?DBG("Index maps: ~p + ~p => ~p~n", [Cmap,lists:sort(Alternatives),Cmap + Offset]),
            Cmap + Offset
    end.

find_index_map1(0,  _, Res) -> Res;
find_index_map1(N, <<?PLATFORM_ID_MICROSOFT:?U16, Enc:?U16, Offset:?U32, Rest/binary>>, Prev) ->
    case Enc of
        ?MS_EID_UNICODE_BMP ->
            find_index_map1(N-1, Rest, [{5, Offset}|Prev]);
        ?MS_EID_UNICODE_FULL ->
            find_index_map1(N-1, Rest, [{1, Offset}|Prev]);
        _ -> %% For example ?MS_EID_SYMBOL
            ?DBG("Ignored: ~w ~p~n",[Enc, encoding(Enc, microsoft)]),
            find_index_map1(N-1, Rest, Prev)
    end;
find_index_map1(N, <<?PLATFORM_ID_UNICODE:?U16, Enc:?U16, Offset:?U32, Rest/binary>>, Prev) ->
    case Enc of
        5 -> %% Cmap format 14 (we don't support that)
            ?DBG("Ignored: ~w ~p~n",[Enc, encoding(Enc, unicode)]),
            find_index_map1(N-1, Rest, Prev);
        4 ->
            find_index_map1(N-1, Rest, [{0, Offset}|Prev]);
        3 ->
            find_index_map1(N-1, Rest, [{4, Offset}|Prev]);
        6 ->
            find_index_map1(N-1, Rest, [{2, Offset}|Prev]);
        _ ->
            find_index_map1(N-1, Rest, [{6, Offset}|Prev])
    end;
find_index_map1(NumTables, <<_:64, Next/binary>>, Res) ->
    find_index_map1(NumTables-1, Next, Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Converts UnicodeCodePoint to Glyph index
%% Glyph 0 is the undefined glyph
-spec find_glyph_index(Font::ttf(), Char::integer()) -> Glyph::integer().
find_glyph_index(#ttf_info{data=Bin, index_map=IndexMap, os2=_Os2}, UnicodeCP) ->
    <<_:IndexMap/binary, Fmt:?U16, Data/binary>> = Bin,
    %% ?DBG("Index map format: ~w @~w~n",[Fmt, IndexMap]),

    %% <<_:_Os2/binary,_Ver:16,_:16,_Weight:?U16,_Pad:26/binary,
    %%   _Panose:10/binary,R1:32,R2:32,R3:32,R4:32,?SKIP>> = Bin,
    %% ?DBG("Support: ~.2b ~.2b ~.2b ~.2b~n",[R1, R2, R3, R4]),

    find_glyph_index(Fmt, Data, UnicodeCP).

find_glyph_index(0, IndexMap, UnicodeCP) ->
    <<Bytes:?U16, _:16, _:UnicodeCP/binary, Index:8, ?SKIP>> = IndexMap,
    %% Format0: Apple byte encoding
    case UnicodeCP < (Bytes-6) of
        true -> Index;
        false -> ?DBG("No CP in range",[]), 0
    end;
find_glyph_index(4, IndexMap, UnicodeCP) ->
    %% Format4: 16 bit mapping
    <<_Len:16, _Lan:16, Format4/binary>> = IndexMap,
    format_4_index(Format4, UnicodeCP);
find_glyph_index(6, IndexMap, UnicodeCP) ->
    %% Format6: Dense 16 bit mapping
    <<_Len:16, _Lang:16, First:?U16, Count:?U16, IndexArray/binary>> = IndexMap,
    case UnicodeCP >= First andalso UnicodeCP < (First+Count) of
        false -> ?DBG("No CP in index range",[]), 0;
        true  ->
            Pos = (UnicodeCP - First)*2,
            <<_:Pos/binary, Index:?U16, ?SKIP>> = IndexArray,
            Index
    end;
find_glyph_index(Format, IndexMap, UnicodeCP)
  when Format =:= 12; Format =:= 13 ->
    %% Format12/13: Mixed 16/32 and pure 32 bit mappings
    <<_:16, _:32, _:32, Count:?U32, Groups/binary>> = IndexMap,
    format_32_search(0, Count, Groups, UnicodeCP, Format);
find_glyph_index(_Format, _IndexMap, _UnicodeCP) ->
    %% Format2: Mixed 8/16 bits mapping for Japanese, Chinese and Korean
    %% Format8: Mixed 16/32 and pure 32 bit mappings
    %% Format10: Mixed 16/32 and pure 32 bit mappings
    ?DBG("unsupported glyph format ~w~n",[_Format]),
    0.

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

    %% Ranges = lists:zip([Code || << Code:?U16 >> <= StartCode],
    %%                    [Code || << Code:?U16 >> <= EndCode]),
    %% Chars = [B-A+1 || {A,B} <- Ranges],
    %% ?DBG("~w~n",[Ranges]),
    %% ?DBG("~p ~w ~n",[lists:sum(Chars), Chars]),

    %% they lie from endCount .. endCount + segCount
    %% but searchRange is the nearest power of two, so...
    RangeShift = SegCountX2 - SearchRange0,
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
            %% ?DBG("Unicode: ~w start ~w~n",[Unicode, Start]),
	    0;
	Offset =:= 0 ->
            <<_:Item/binary, Index:?S16, ?SKIP>> = IdDelta,
	    (Index + Unicode) rem 65536;
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
  when Glyph > NumGlyphs ->
    ?DBG("Out of range ~p max: ~p~n",[Glyph, NumGlyphs]),
    -1;
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
get_glyf_offset(#ttf_info{index_to_loc_format=_F}, _) ->
    ?DBG("unknown glyph map format: ~p~n",[_F]),
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
-spec scale_for_mapping_em_to_pixels(Font::ttf(), Size::number()) -> Scale::float().
scale_for_mapping_em_to_pixels(#ttf_info{data=Bin, head=Head}, Size) ->
    <<_:Head/binary, _:18/binary, UnitsPerEm:?U16, ?SKIP>> = Bin,
    Size / UnitsPerEm.

%% -spec get_glyph_box(Font::ttf(), Glyph::integer()) ->
%% 			   {X0::integer(),Y0::integer(),
%% 			    X1::integer(),Y1::integer()}.
%% get_glyph_box(TTF = #ttf_info{data=Bin, glyf=Glyf}, Glyph)
%%   when Glyf =/= undefined ->
%%     case get_glyf_offset(TTF, Glyph) of
%% 	Offset when Offset > 0  ->
%% 	    <<_:Offset/binary, _:16, X0:?S16, Y0:?S16, X1:?S16, Y1:?S16, ?SKIP>> = Bin,
%% 	    {X0,Y0,X1,Y1};
%% 	_ ->
%% 	    {0,0,0,0}
%%     end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_glyph_shape(Font::ttf(), Glyph::integer()) -> [Vertices::vertex()].
get_glyph_shape(#ttf_info{glyf=undefined}=TTF, Glyph) ->
    get_glyph_shape_tt2(TTF, Glyph);
get_glyph_shape(TTF, Glyph) ->
    get_glyph_shape_tt(TTF, Glyph, get_glyf_offset(TTF, Glyph)).

get_glyph_shape_tt(_TTF, _Glyph, Offset)
  when Offset < 0 -> [];
get_glyph_shape_tt(TTF = #ttf_info{data=Bin}, _Glyph, Offset) ->
    <<_:Offset/binary, NumberOfContours:?S16,
      _XMin:16, _YMin:16, _XMax:16, _YMax:16,
      GlyphDesc/binary>> = Bin,

    %% ?DBG("Glyph: ~p ~p c#~p ~p ~p~n",[_Glyph, Offset, NumberOfContours, {_XMin,_XMax}, {_YMin,_YMax}]),

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
	    %% Several Glyphs (Compound shapes)
	    get_glyph_shapes(GlyphDesc, TTF, []);
       NumberOfContours < -1 ->
	    throw({error, bad_ttf, ?__(1, "Unsupported TTF format")});
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
	true when Fs0 =/= [] ->
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
	_ ->
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
    %% io:format(" ~p ~p ~p~n",[Mode, Pos, C]),
    [#vertex{type=Mode, pos=Pos, c=C}|Vs].
set_vertex(Vs, Mode, Pos, C, C1) ->
    %% io:format(" ~p ~p ~p ~p~n", [Mode, Pos, C, C1]),
    [#vertex{type=Mode, pos=Pos, c=C, c1=C1}|Vs].

get_glyph_shapes(<<Flags:?S16, GidX:?S16, GlyphDesc0/binary>>, Font, Vs0) ->
    {ScaleInfo,GlyphDesc} = find_trans_scales(Flags, GlyphDesc0),
    Vs1 = get_glyph_shape(Font, GidX),
    Vs = scale_vertices(Vs1, ScaleInfo, Vs0),
    case (Flags band (1 bsl 5)) > 1 of
	true -> %% More Components
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
%% CFF stuff

pp_cff(undefined, _Bin, _File) ->
    undefined;
pp_cff(CffIdx, Bin0, _DbgFontFile) ->
    <<_:CffIdx/binary,Cff/binary>> = Bin0,
    <<1, _MinorVsn, HeaderSz, ?SKIP>> = Cff,
    <<_:HeaderSz/binary, Cont0/binary>> = Cff,
    {_NameIdx,  Cont1} = cff_index(Cont0),
    {TopDictIdx, Cont2} = cff_index(Cont1),
    TopDict = get_cff_index(0, TopDictIdx),  %% OpenType only supports one
    %?DBG("Strings: ~.16b~n",[byte_size(Cff) - byte_size(Cont2)]),
    {_StringIdx, Cont3} = cff_index(Cont2),
    %?DBG("Global Subrs: ~.16b~n",[byte_size(Cff) - byte_size(Cont3)]),
    {Gsubrs, _Cont4} = cff_index(Cont3),
    CharStringsOff = get_cff_dict(17,TopDict),
    CsType = get_cff_dict({12,6},TopDict),
    FdArrayOff = get_cff_dict({12,36},TopDict),
    FdSelectOff = get_cff_dict({12,37},TopDict),
    if CsType =:= 2; CsType =:= [], CharStringsOff /= [] -> ok;
       true -> throw({error, bad_cff_cstype, ?__(1, "Not supported OTF (cff) format")})
    end,

    Fd = case FdArrayOff of
             [] -> undefined;
             _ when FdSelectOff =/= [] ->  %% Looks like a CID font?
                 <<_:FdArrayOff/binary, FdArrBin/binary>> = Cff,
                 {FontDicts, _} = cff_index(FdArrBin),
                 %% ?DBG("~s: ~p ~p ~p~n",[_DbgFontFile, FdArrayOff, FdSelectOff, size(Cff)]),
                 <<_:FdSelectOff/binary, FdSelectBin/binary>> = Cff,
                 {FontDicts, FdSelectBin};
             _ ->
                 throw({error, no_fd_select, ?__(1, "Not supported OTF (cff) format")})
         end,
    Subrs = cff_get_subrs(TopDict, Cff),
    %% ?DBG("ChStr ~p type ~p FdA ~p FdS ~p ~p~n",
    %%      [CharStringsOff, CsType, FdArrayOff, FdSelectOff, size(Cff)]),
    <<_:CharStringsOff/binary, CharSBin/binary>> = Cff,
    {CharStrings, _Rest} = cff_index(CharSBin),
    #{topDict => TopDict,
      gsubrs  => Gsubrs,
      subrs   => Subrs,
      charStrings => CharStrings,
      fdSel => Fd,
      cff => Cff
     }.

cff_get_subrs(Dict, Bin) ->
    case get_cff_dict(18, Dict) of
        [Size, Offset] when Size /= 0, Offset /= 0 ->
            %% ?DBG("~p ~p ~.16b~n",[Size, Offset, Offset]),
            <<_:Offset/binary, PrivDict:Size/binary, _Rest/binary>> = Bin,
            case get_cff_dict(19, PrivDict) of
                [] -> undefined;
                SubrsOffs0 ->
                    SubrsOffs = SubrsOffs0 + Offset,
                    <<_:SubrsOffs/binary, Subrs/binary>> = Bin,
                    {Index, _} = cff_index(Subrs),
                    Index
            end;
        _ ->
            undefined
    end.

cff_index(<<0:16, Rest0/binary>>) ->
    {undefined, Rest0};
cff_index(<<Count:16, OffSz:8, Rest0/binary>>) when 1 =< OffSz, OffSz =< 4 ->
    TabSz = OffSz*(Count+1), Bits = OffSz*8,
    %% io:format("~p ~p ~p ~p~n", [Count, OffSz, TabSz, Bits]),
    <<Offsets0:TabSz/binary, Rest1/binary>> = Rest0,
    Offsets = [OS-1 || << OS:Bits >> <= Offsets0],
    Last = lists:last(Offsets),
    %% io:format("~p ~p ~p ~p ~p~n", [Count, OffSz, TabSz, Bits, Last]),
    <<_:Last/binary, Rest/binary>> = Rest1,
    {{array:from_list(Offsets), Rest1}, Rest}.

get_cff_index(Idx, {Offsets, Bin}) ->
    Offset = array:get(Idx, Offsets),
    Sz = array:get(Idx+1, Offsets) - Offset,
    <<_:Offset/binary, Data:Sz/binary, ?SKIP>> = Bin,
    Data.

get_cff_index_count({Offsets, _Bin}) ->
    array:size(Offsets).

get_cff_dict(Key, Bin) ->
    get_ccf_dict(Bin, [], Key).

get_ccf_dict(<<Data, ?SKIP>>=Bin, Vals, Key)
  when Data >= 28 ->
    {Val, Rest} = ccf_dict_operand(Bin),
    get_ccf_dict(Rest, [Val|Vals], Key);
get_ccf_dict(<<Key, ?SKIP>>, Val, Key) ->
    dict_val(Val);
get_ccf_dict(<<12, Op, Rest/binary>>, Val, Key) ->
    case Key of
        {12,Op} -> dict_val(Val);
        _  -> get_ccf_dict(Rest, [], Key)
    end;
get_ccf_dict(<<_Key, Rest/binary>>, _Val, Key) ->
    %% io:format("Not found ~p: ~p~n",[_Key,_Val]),
    get_ccf_dict(Rest, [], Key);
get_ccf_dict(<<>>, [], _) ->
    [].

dict_val([{float, _Bin}]) ->
    throw({nyi, cff_float});
dict_val([Val]) ->
    Val;
dict_val(Vals) when is_list(Vals) ->
    lists:reverse(Vals).

ccf_dict_operand(<<28, Val:?S16, Rest/binary>>) ->
    {Val,Rest};
ccf_dict_operand(<<29, Val:?S32, Rest/binary>>) ->
    {Val,Rest};
ccf_dict_operand(<<30, Bin/binary>>) ->
    Rest = ccf_dict_skip_float(Bin),
    %% Sz = byte_size(Bin) - byte_size(Rest),
    %% <<FloatBin:Sz, ?SKIP>> = Rest,
    {{float, aFloatBin}, Rest};
ccf_dict_operand(<<B, Rest/binary>>)
  when 31 < B, B < 247 ->
    {B-139, Rest};
ccf_dict_operand(<<B0, B1, Rest/binary>>)
  when 31 < B0, B0 < 255 ->
    case B0 < 251 of
        true  -> {(B0-247)*256+B1+108, Rest};
        false -> {-(B0-251)*256-B1-108, Rest}
    end.

ccf_dict_skip_float(<<16#F:4, _:4, Rest/binary>>) -> Rest;
ccf_dict_skip_float(<<_:4, 16#F:4, Rest/binary>>) -> Rest;
ccf_dict_skip_float(<<_, Rest/binary>>) ->
    ccf_dict_skip_float(Rest).

get_glyph_subrs(Glyph, {FontDicts, <<Fmt, FdSelBin/binary>>}, Cff) ->
    FdSel = case Fmt of
                0 -> %% Untested !!!
                    <<_:Glyph/binary, FdSelByte, ?SKIP>> = FdSelBin,
                    FdSelByte;
                3 ->
                    <<NRanges:?S16, Start:?S16, Cont0/binary>> = FdSelBin,
                    get_glyph_fd(NRanges, Start, Glyph, Cont0)
            end,
    %% ?DBG("Subr Format ~p sel: ~p index: ~p~n",[Fmt, FdSel, get_cff_index(FdSel, FontDicts)]),
    cff_get_subrs(get_cff_index(FdSel, FontDicts), Cff).

get_glyph_fd(N, Start, Glyph, Bin) when N > 0 ->
    <<V:8, End:?U16, Cont/binary>> = Bin,
    case Start =< Glyph andalso Glyph < End of
        true  -> V;
        false -> get_glyph_fd(N-1, End, Glyph, Cont)
    end;
get_glyph_fd(_, _, _, _) ->
    throw({error, fd_not_found, "Not supported"}).

get_glyph_shape_tt2(#ttf_info{cff=CFF} = _TTF, Glyph) ->
    run_charstring(CFF, Glyph).

run_charstring(#{charStrings := CharSs}=Cff,  Glyph) ->
    Ops = get_cff_index(Glyph, CharSs),
    State = Cff#{ %% Add the following temporary variables
                  in_header => true,
                  maskbits => 0,
                  has_subrs => false,
                  glyph => Glyph
                },
    %% ?DBG("Glyph: ~w ~P~n",[Glyph, Ops, 40]),
    {return, {_,_,All}} = run_chars(Ops, [], State, csctx_new()),
    lists:reverse(All).

run_chars(Bin, Stack, State, Acc) ->
    %% ?DBG("~.16b (~w) ~w~n", [binary:first(Bin), length(Stack), Stack]),
    do_run_chars(Bin, Stack, State, Acc).

do_run_chars(<<16#13, Rest0/binary>>, Stack, #{maskbits:=MB0, in_header:=InH}=State, Acc)->
    %% Hintmask NYI
    MB = case InH of
             true ->  (length(Stack) div 2) + MB0;
             false -> MB0
         end,
    Skip = (MB + 7) div 8,
    <<_:Skip/binary, Rest/binary>> = Rest0,
    run_chars(Rest, [], State#{maskbits:=MB,in_header:=false}, Acc);
do_run_chars(<<16#14, Rest0/binary>>, Stack, #{maskbits:=MB0, in_header:=InH}=State, Acc) ->
    %% CNTRMASK
    MB = case InH of
             true  -> (length(Stack) div 2) + MB0;
             false -> MB0
         end,
    Skip = (MB + 7) div 8,
    <<_:Skip/binary, Rest/binary>> = Rest0,
    run_chars(Rest, [], State#{maskbits:=MB,in_header=>false}, Acc);
do_run_chars(<<Stem, Rest/binary>>, Stack, #{maskbits:=MB0}=State, Acc)
  when Stem =:= 16#01;   %% hstem
       Stem =:= 16#03;   %% vstem
       Stem =:= 16#12;   %% hstemhm
       Stem =:= 16#17 -> %% vstemhm
    MB = (length(Stack) div 2) + MB0,
    run_chars(Rest, [], State#{maskbits:=MB}, Acc);

do_run_chars(<<16#15, Rest/binary>>, [S2,S1|_], State, Acc0) ->
    %% rmoveto
    Acc = csctx_rmove_to(S1,S2,Acc0),
    run_chars(Rest, [], State#{in_header:=false}, Acc);
do_run_chars(<<16#04,Rest/binary>>, [S1|_], State, Acc0) ->
    %% vmoveto
    Acc = csctx_rmove_to(0, S1, Acc0),
    run_chars(Rest, [], State#{in_header:=false}, Acc);
do_run_chars(<<16#16,Rest/binary>>, [S1|_], State, Acc0) ->
    %% hmoveto
    Acc = csctx_rmove_to(S1, 0, Acc0),
    run_chars(Rest, [], State#{in_header:=false}, Acc);

do_run_chars(<<16#05,Rest/binary>>, Stack, State, Acc0) ->
    Acc = rlineto(reverse(Stack), Acc0),
    run_chars(Rest, [], State#{in_header:=false}, Acc);

do_run_chars(<<16#06,Rest/binary>>, Stack, State, Acc0) ->
    Acc = hlineto(reverse(Stack), Acc0),
    run_chars(Rest, [],  State#{in_header:=false}, Acc);
do_run_chars(<<16#07,Rest/binary>>, Stack, State, Acc0) ->
    Acc = vlineto(reverse(Stack), Acc0),
    run_chars(Rest, [],  State#{in_header:=false}, Acc);

do_run_chars(<<16#1E,Rest/binary>>, Stack, State, Acc0) ->
    Acc = vhcurveto(reverse(Stack), Acc0),
    run_chars(Rest, [],  State, Acc);
do_run_chars(<<16#1F,Rest/binary>>, Stack, State, Acc0) ->
    Acc = hvcurveto(reverse(Stack), Acc0),
    run_chars(Rest, [],  State, Acc);
do_run_chars(<<16#08,Rest/binary>>, Stack, State, Acc0) ->
    Acc = rrcurveto(reverse(Stack), Acc0),
    run_chars(Rest, [],  State, Acc);

do_run_chars(<<16#18,Rest/binary>>, Stack, State, Acc0) ->
    Acc = rrcurveline(reverse(Stack), Acc0),
    run_chars(Rest, [],  State, Acc);
do_run_chars(<<16#19,Rest/binary>>, Stack, State, Acc0) ->
    Acc = rrlinecurve(reverse(Stack), Acc0),
    run_chars(Rest, [],  State, Acc);

do_run_chars(<<16#1A,Rest/binary>>, Stack0, State, Acc0) ->
    [F|Stack1] = Stack = reverse(Stack0),
    Acc = case length(Stack) rem 2 of
              1 -> vvcurveto(Stack1, F, Acc0);
              0 -> vvcurveto(Stack, 0.0, Acc0)
          end,
    run_chars(Rest, [],  State, Acc);
do_run_chars(<<16#1B,Rest/binary>>, Stack0, State, Acc0) ->
    [F|Stack1] = Stack = reverse(Stack0),
    Acc = case length(Stack) rem 2 of
              1 -> hhcurveto(Stack1, F, Acc0);
              0 -> hhcurveto(Stack, 0.0, Acc0)
          end,
    run_chars(Rest, [],  State, Acc);

do_run_chars(<<16#0A,Rest/binary>>, [V|Stack0], State0, Acc0) ->
    {State1,Subrs} =
        case maps:get(has_subrs, State0) orelse maps:get(fdSel,State0, undefined) of
            true -> {State0, maps:get(subrs, State0)};
            undefined -> {State0, maps:get(subrs, State0)};
            FdSel ->
                #{cff := Cff, glyph := Glyph} = State0,
                GlSubrs = get_glyph_subrs(Glyph, FdSel, Cff),
                {State0#{has_subrs := true, subrs := GlSubrs}, GlSubrs}
        end,
    %%?DBG("Recurse ...~p~n",[Stack0]),
    case callsubr(V, Subrs, Stack0, State1, Acc0) of
        {subrr, Stack, State, Acc} ->
            %%?DBG("...done ~p~n",[Stack]),
            run_chars(Rest, Stack, State, Acc);
        {return, _} = Acc ->
            %%?DBG("...done return~n",[]),
            <<>> = Rest, %% Assert
            Acc
    end;
do_run_chars(<<16#1D,Rest/binary>>, [V|Stack0], #{gsubrs := GSubrs} = State0, Acc0) ->
    %%?DBG("Recurse ...~p~n",[Stack0]),
    case callsubr(V, GSubrs, Stack0, State0, Acc0) of
        {subrr, Stack, State, Acc} ->
            %%?DBG("...done ~p~n",[Stack]),
            run_chars(Rest, Stack, State, Acc);
        {return, _} = Acc ->
            %%?DBG("...done return~n",[]),
            <<>> = Rest, %% Assert
            Acc
    end;

do_run_chars(<<16#0B,Rest/binary>>, Stack, State, Acc) ->
    %% Return (subr)
    <<>> = Rest,
    {subrr, Stack, State, Acc};
do_run_chars(<<16#0E,Rest/binary>>, _, _State, Acc) ->
    %% endchar
    <<>> = Rest,
    {return, csctx_close_shape(Acc)};

do_run_chars(<<16#0C,B1,Rest/binary>>, Stack, State, Acc0) ->
    %% Two-byte Escape-seq
    Acc = run_char(B1,Stack,Acc0),
    run_chars(Rest, [], State, Acc);
do_run_chars(<<16#FF, Int:32, Rest/binary>>, Stack, State, Acc) ->
    run_chars(Rest, [Int/16#10000|Stack], State, Acc);
do_run_chars(Bin, Stack, State, Acc) ->
    try ccf_dict_operand(Bin) of
        {Val, Rest} ->
            run_chars(Rest, [Val|Stack], State, Acc)
    catch _:_ ->
            <<Byte, _/binary>> = Bin,
            io:format("Bad op: 16#~.16b ~p~n ~P~n ~P~nin ~P~n",
                      [Byte, Stack, State, 10, Acc, 20, Bin, 40]),
            throw({error, parse_error})
    end.

run_char(16#22, [Dx6,Dx5,Dx4,Dx3,Dy2,Dx2,Dx1], Acc0) ->
    %% hflex
    Acc = csctx_rccurve_to(Dx1,0,Dx2,Dy2,Dx3,0,Acc0),
    csctx_rccurve_to(Dx4,0,Dx5,-Dy2,Dx6,0,Acc);
run_char(16#23, [_Fd, Dy6,Dx6,Dy5,Dx5,Dy4,Dx4,Dy3,Dx3,Dy2,Dx2,Dy1,Dx1], Acc0) ->
    %% flex
    Acc = csctx_rccurve_to(Dx1,Dy1,Dx2,Dy2,Dx3,Dy3,Acc0),
    csctx_rccurve_to(Dx4,Dy4,Dx5,Dy5,Dx6,Dy6,Acc);
run_char(16#24, [Dx6,Dy5,Dx5,Dx4,Dx3,Dy2,Dx2,Dy1,Dx1], Acc0) ->
    %% hflex1
    Acc = csctx_rccurve_to(Dx1,Dy1,Dx2,Dy2,Dx3,0,Acc0),
    csctx_rccurve_to(Dx4,0,Dx5,Dy5,Dx6,-(Dy1+Dy2+Dy5),Acc);
run_char(16#25, [D6,Dy5,Dx5,Dy4,Dx4,Dy3,Dx3,Dy2,Dx2,Dy1,Dx1], Acc0) ->
    %% flex1
    Dx = Dx1+Dx2+Dx3+Dx4+Dx5,
    Dy = Dy1+Dy2+Dy3+Dy4+Dy5,
    {Dx6,Dy6} = case abs(Dx) > abs(Dy) of
                    true  -> {D6, -Dy};
                    false -> {-Dx, D6}
                end,
    Acc = csctx_rccurve_to(Dx1,Dy1,Dx2,Dy2,Dx3,Dy3,Acc0),
    csctx_rccurve_to(Dx4,Dy4,Dx5,Dy5,Dx6,Dy6,Acc).

callsubr(N0, Subrs, Stack, State, Acc) ->
    Count = get_cff_index_count(Subrs),
    Bias = if Count >= 33900 -> 32768;
              Count >= 1240  -> 1131;
              true -> 107
           end,
    N = N0+Bias,
    ((N < 0) orelse (N >= Count)) andalso
        throw({error, internal_error, "A bug is found"}),
    Bin = get_cff_index(N, Subrs),
    %% ?DBG("sub ~W~n",[Bin, 40]),
    run_chars(Bin, Stack, State, Acc).

rlineto([S0,S1|St], Acc) ->
    rlineto(St, csctx_rline_to(S0,S1,Acc));
rlineto([], Acc) -> Acc.

%% Note: hlineto/vlineto alternate horizontal and vertical
%% starting from a different place.
hlineto([S0|St], Acc0) ->
    vlineto(St, csctx_rline_to(S0, 0, Acc0));
hlineto([], Acc) -> Acc.

vlineto([S0|St], Acc0) ->
    hlineto(St, csctx_rline_to(0, S0, Acc0));
vlineto([], Acc) -> Acc.

%% Note: vhcurveto/hvcurveto alternate horizontal and vertical
%% starting from a different place.
vhcurveto([S0,S1,S2,S3|St], Acc0) ->
    S4 = case St of
             [Last]-> Last;
             _ -> 0.0
         end,
    Acc = csctx_rccurve_to(0, S0, S1, S2, S3, S4, Acc0),
    hvcurveto(St, Acc);
vhcurveto(_, Acc) -> Acc.

hvcurveto([S0,S1,S2,S3|St], Acc0) ->
    S4 = case St of
             [Last] -> Last;
             _ -> 0.0
         end,
    Acc = csctx_rccurve_to(S0, 0, S1, S2, S4, S3, Acc0),
    vhcurveto(St, Acc);
hvcurveto(_, Acc) -> Acc.

rrcurveto([S0,S1,S2,S3,S4,S5|St], Acc0) ->
    Acc = csctx_rccurve_to(S0,S1,S2,S3,S4,S5,Acc0),
    rrcurveto(St,Acc);
rrcurveto(_, Acc) -> Acc.

rrcurveline([S0,S1,S2,S3,S4,S5|St], Acc0) ->
    Acc = csctx_rccurve_to(S0,S1,S2,S3,S4,S5,Acc0),
    rrcurveline(St, Acc);
rrcurveline([S0,S1], Acc0) ->
    csctx_rline_to(S0, S1, Acc0).

rrlinecurve([S0,S1|St], Acc0)
  when length(St) >= 6 ->
    Acc = csctx_rline_to(S0, S1, Acc0),
    rrlinecurve(St, Acc);
rrlinecurve([S0,S1,S2,S3,S4,S5], Acc0) ->
    csctx_rccurve_to(S0,S1,S2,S3,S4,S5,Acc0).

vvcurveto([S0,S1,S2,S3|St], F, Acc0) ->
    Acc = csctx_rccurve_to(F,S0,S1,S2,0.0,S3,Acc0),
    vvcurveto(St, 0.0, Acc);
vvcurveto([], _, Acc) -> Acc.

hhcurveto([S0,S1,S2,S3|St], F, Acc0) ->
    Acc = csctx_rccurve_to(S0,F,S1,S2,S3,0.0,Acc0),
    hhcurveto(St, 0.0, Acc);
hhcurveto([], _, Acc) -> Acc.

csctx_new() ->
    {{0.0,0.0},{0.0,0.0},[]}.

csctx_close_shape({{Fx,Fy}=First, {X,Y}=XY, Shapes} = Acc) ->
    case Fx /= X orelse Fy /= Y of
        true ->
            {First, XY, set_vertex(Shapes, line, First, {0,0})};
        false ->
            Acc
    end.

csctx_rmove_to(Dx,Dy,Acc0) ->
    {_First, {X,Y}, Shs} = csctx_close_shape(Acc0),
    XY = {X+Dx,Y+Dy},
    {XY, XY, set_vertex(Shs, move, XY, {0,0})}.

csctx_rline_to(Dx,Dy,{First,{X,Y},Shs}) ->
    XY = {X+Dx,Y+Dy},
    {First, XY, set_vertex(Shs, line, XY, {0,0})}.

csctx_rccurve_to(Dx1,Dy1,Dx2,Dy2,Dx3,Dy3,{First,{X,Y},Shs}) ->
    Cx1 = X + Dx1,
    Cy1 = Y + Dy1,
    Cx2 = Cx1 + Dx2,
    Cy2 = Cy1 + Dy2,
    XY  = {Cx2 + Dx3,Cy2 + Dy3},
    {First, XY, set_vertex(Shs, cubic, XY, {Cx1,Cy1}, {Cx2,Cy2})}.


