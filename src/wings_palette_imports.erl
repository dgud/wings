%%
%%  wings_palette_imports.erl --
%%
%%     Import and export palettes from different sources
%%
%%  Copyright (c) 2004-2023 Bjorn Gustavsson, Dan Gudmundsson
%%                2023 Edward Blake (more palettes), Micheus Vieira (more compact code)
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

%% Thanks to dgud, micheus and tkbd for suggestions and testing
%% 

-module(wings_palette_imports).
-export([palette_menu/1,command/2]).

-include_lib("wings/src/wings.hrl").
-include_lib("wings/e3d/e3d.hrl").
-include_lib("wings/e3d/e3d_image.hrl").

palette_menu([]) -> [];
palette_menu([{Label,{import,List}}|R]) ->
    [{Label,{import,List ++ menu_entry_import()}}
    ] ++ palette_menu(R);
palette_menu([{Label,{export,List}}|R]) ->
    [{Label,{export,List ++ menu_entry_export()}}
    ] ++ palette_menu(R);
palette_menu([Menu|R]) ->
    [Menu|palette_menu(R)].

menu_entry_import() ->
    [
        separator,
        {?__(4,"Adobe Palette (.aco|.ase|...)"),aco},
        {?__(1,"From swatch image (.png|.tif)"),swimg},
        {?__(2,"Text file (.txt|.css)"),text},
        separator,
        {?__(6,"Sample from image (.png|.jpg|.tif)"),samp_img},
        {?__(7,"Sample from texture"),samp_tex}
    ].

menu_entry_export() ->
    [
        separator,
        {?__(2,"EPS swatch image (.eps)"),swimg_eps},
        {?__(1,"SVG swatch image (.svg)"),swimg_svg},
        {?__(5,"SVG legend (.svg)"),legend_svg},
        {?__(3,"Text file (Columns) (.txt)"),text_columns},
        {?__(4,"Text file (Hex) (.txt)"),text_hex},
        {?__(6,"TeX+TikZ legend (.tex)"),legend_tex}
    ].


command({palette,{import, Kind}}, #st{pal=Cols}=St) ->
    import(St, Kind, Cols);
command({palette,{export, Kind}}, #st{pal=Cols}=St) ->
    export(St, Kind, Cols);
command(_, _) ->
    next.

import_filename(Ps0,Fun) ->
    Dir = wings_pref:get_value(current_directory),
    Ps = Ps0 ++ [{directory,Dir}],
    wings_plugin:call_ui({file,open_dialog,Ps,Fun}).

import(St, swimg=Kind, Pal) ->
    empty_import_warning(Pal, fun() ->
        Ps = [{extensions,[{".png",?__(21,"PNG Image")},
                           {".tif",?__(22,"TIFF Image")}]},
              {title,?__(20,"Import palette from swatch image")}],
        import_filename(Ps, do_import_palette(Kind, Pal, St))
   end);
import(St, text=Kind, Pal) ->
    empty_import_warning(Pal, fun() ->
        Ps = [{extensions,[{".txt", ?__(24,"Text file")},
                           {".css",?__(25,"Style sheet")}]},
              {title,?__(23,"Import palette from text file")}],
        import_filename(Ps, do_import_palette(Kind, Pal, St))
    end);
import(St, aco=Kind, Pal) ->
    empty_import_warning(Pal, fun() ->
        Ps = [{extensions,[{".aco",?__(27,"Adobe color palette")},
                           {".ase",?__(28,"Adobe color swatch")},
                           {".acb",?__(29,"Adobe color book")},
                           {".act",?__(30,"Adobe color table")}]},
              {title,?__(26,"Import Adobe palette")}],
        import_filename(Ps, do_import_palette(Kind, Pal, St))
   end);
import(St, samp_img=Kind, Pal) ->
    empty_import_warning(Pal, fun () ->
        Ps = [{extensions,[{".png",?__(32,"PNG Image")},
                           {".jpg",?__(33,"JPEG Image")},
                           {".tif",?__(34,"TIFF Image")}]},
              {title,?__(31,"Sample colors from image file")}],
        import_filename(Ps, do_import_palette(Kind, Pal, St))
    end);
import(St, samp_tex, Pal) ->
    case texture_images() of
        [{First,_}|_]=List ->
            empty_import_warning(Pal, fun () ->
                do_import_samp_texture(Pal, First, List, St)
                                       end);
        _ ->
            wpa:error_msg(?__(35,"There are no textures")),
            St
    end.


do_import_palette(Kind, Pal, St) ->
    Fun0 =
    case Kind of
        swimg -> fun read_swimg/1;
        text -> fun read_txtpal/1;
        samp_img -> fun read_samp_file/1;
        aco -> fun read_aco/1
    end,

    fun(Filename) ->
        Fun =
        case Kind of
            aco ->
                case string:lowercase(filename:extension(Filename)) of
                    ".act" -> fun read_act/1;
                    ".acb" -> fun read_acb/1;
                    ".ase" -> fun read_ase/1;
                    _      -> fun read_aco/1
                end;
            _ -> Fun0
        end,
        try Fun(Filename) of
            {palette, L} when length(L) > 0 ->
                wings_wm:send(geom, {new_state,St#st{pal=remove_none(Pal) ++ L}});
            _ ->
                keep
        catch _:E:StT ->
            io:format("~w: Error: ~p stack trace: ~p~n", [?MODULE, E, StT]),
            wpa:error_msg(?__(1,"An error occurred while trying to import.")),
            keep
        end
    end.


do_import_samp_texture(Pal0, First, List, St) ->
    Dialog = dialog_list(List, First),
    Fun =
        fun([Id|_]) ->
            {_Name, Img} = proplists:get_value(Id, List),
            try read_samp_img(Img) of
                {palette, L} when length(L) > 0 ->
                    wings_wm:send(geom, {new_state,St#st{pal=remove_none(Pal0) ++ L}});
                _ ->
                    keep
            catch _:E:StT ->
                io:format("~w: Error: ~p stack trace: ~p~n", [?MODULE, E, StT]),
                wpa:error_msg(?__(19,"An error occurred while trying to import.")),
                St
            end
        end,
    wings_dialog:dialog(?__(36,"Choose Texture"), Dialog, Fun).

texture_images() ->
    List = [{list_to_atom(Name), {Name, Img}}
                || {_, #e3d_image{name=Name}=Img} <- wings_image:images()],
    List.

dialog_list(List, First) ->
    [
        {vframe, [
            {label, ?__(1, "Texture:")},
            {menu, [{Name,Id} || {Id,{Name,_}} <- List], First, []}
        ]}
    ].



empty_import_warning([], Fun) ->
    Qs = ?__(1, "The current palette had not been modified, which means an \n"
                "imported file will replace all the colors, is that ok?"),
    wpa:yes_no(Qs, Fun);
empty_import_warning(_, Fun) ->
    Fun().

%%%
%%%

%%% Swatch image

%%
%% Add to the palette colors of pixels where adjacent pixels are of the same color,
%% and ignore the ones that don't have adjacent identical colors, which may be
%% anti-aliasing artifacts.
%%

%% 3 pixel or higher swatch image mode

-define(PXL_AT(X, Y, W, Channels), ((((Y) * W) + (X)) * Channels)).

read_swimg(Filename) ->
    case load_image_e3d(Filename) of
        #e3d_image{image=IB,width=IW,height=IH,type=Type}
          when IH >= 3, IW >= 3 ->
            %% A swatch image we can scan with a 3x3 area.
            case Type of
                r8g8b8 ->
                    scan_swimg(IB, 3, IW, IH);
                r8g8b8a8 ->
                    scan_swimg(IB, 4, IW, IH)
            end;
        #e3d_image{image=IB,width=IW,height=IH,type=Type}
          when IH < 3; IW < 3 ->
            %% A swatch image that is smaller than 3 pixels in height
            %% or width, it will be scanned at 1 pixel per color.
            case Type of
                r8g8b8 ->
                    scan_swimg_1px(IB, 3, IW, IH);
                r8g8b8a8 ->
                    scan_swimg_1px(IB, 4, IW, IH)
            end
    end.

scan_swimg(IB, Channels, IW, IH) ->
    Img = {IB, Channels, IW, IH},
    scan_swimg_rows(Img, IW, IH, 1, 1, sets:new()).

scan_swimg_rows(Img, IW, IH, Col, Row, ColSet)
  when Row < IH-1 ->
    RR = scan_swimg_cols(Img, IW, Col, Row, []),
    ColSet1 = sets:union(sets:from_list(RR), ColSet),
    scan_swimg_rows(Img, IW, IH, Col, Row+1, ColSet1);
scan_swimg_rows(_, _, _, _, _, ColSet) ->
    {palette, sets:to_list(ColSet)}.

scan_swimg_cols(Img, IW, Col, Row, OL)
  when Col < IW-1 ->
    case samp_swimg(Img, Col, Row) of
        false ->
            scan_swimg_cols(Img, IW, Col+1, Row, OL);
        I ->
            scan_swimg_cols(Img, IW, Col+2, Row, [I|OL])
    end;
scan_swimg_cols(_, _, _, _, OL) ->
    lists:reverse(OL).

%% Only take a color if it is surrounded by identical color around it
%% so that anti-aliasing artifacts are less likely to be taken up.
samp_swimg({IB, Channels, IW, IH}, X, Y)
  when X > 0, X < IW-1, Y > 0, Y < IH-1 ->    
    P1=binary:part(IB, ?PXL_AT(X, Y-1, IW, Channels), Channels),
    P2=binary:part(IB, ?PXL_AT(X-1, Y, IW, Channels), Channels),
    P3=binary:part(IB, ?PXL_AT(X, Y, IW, Channels), Channels),
    P4=binary:part(IB, ?PXL_AT(X, Y+1, IW, Channels), Channels),
    P5=binary:part(IB, ?PXL_AT(X+1, Y, IW, Channels), Channels),
    case (P1 =:= P2) andalso 
         (P2 =:= P3) andalso 
         (P3 =:= P4) andalso 
         (P4 =:= P5) of
        true ->
            to_tuple_8b(P3);
        false ->
            false
    end;
samp_swimg(_, _, _) ->
    false.

%% 1 pixel swatch mode

scan_swimg_1px(IB, Channels, IW, IH) ->
    Img = {IB, Channels, IW, IH},
    scan_swimg_1px_rows(Img, IW, IH, 0, 0, sets:new()).

scan_swimg_1px_rows(Img, IW, IH, Col, Row, ColSet)
  when Row < IH ->
    RR = scan_swimg_1px_cols(Img, IW, Col, Row, []),
    ColSet1 = sets:union(sets:from_list(RR), ColSet),
    scan_swimg_1px_rows(Img, IW, IH, Col, Row+1, ColSet1);
scan_swimg_1px_rows(_, _, _, _, _, ColSet) ->
    {palette, sets:to_list(ColSet)}.

scan_swimg_1px_cols(Img, IW, Col, Row, OL)
  when Col < IW ->
    case samp_swimg_1px(Img, Col, Row) of
        false ->
            scan_swimg_1px_cols(Img, IW, Col+1, Row, OL);
        I ->
            scan_swimg_1px_cols(Img, IW, Col+1, Row, [I|OL])
    end;
scan_swimg_1px_cols(_, _, _, _, OL) ->
    lists:reverse(OL).

%% Take the pixel color
samp_swimg_1px({IB, Channels, IW, IH}, X, Y)
  when X >= 0, X < IW, Y >= 0, Y < IH ->    
    P3=binary:part(IB, ?PXL_AT(X, Y, IW, Channels), Channels),
    to_tuple_8b(P3);
samp_swimg_1px(_, _, _) ->
    false.



to_tuple_8b(<<R,G,B,_>>) ->
    {R / 255.0, G / 255.0, B / 255.0};
to_tuple_8b(<<R,G,B>>) ->
    {R / 255.0, G / 255.0, B / 255.0};
to_tuple_8b(<<G,_>>) ->
    {G / 255.0, G / 255.0, G / 255.0};
to_tuple_8b(<<G>>) ->
    {G / 255.0, G / 255.0, G / 255.0}.


%%%
%%%

%% From text file (.txt|.css)
%% Simply scan for colors written as #rrggbb, "rrggbb" and rgb(....)
%% If no colors are found from scanning that way, try instead to read
%% columns of numbers.

read_txtpal(Filename) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            List = binary:split(Content,
                [<<"\r\n">>,<<"\n">>,<<"\r">>], [global]),
            read_txtpal_1(List)
    end.

read_txtpal_1(List) ->
    %% First try by matching for strings in free form mode.
    case txtpal(fun txtpal_f/1, List, []) of
        {palette, L}=Ret when length(L) > 0 ->
            Ret;
        {palette, []} ->
            %% Try again by looking for columnar data
            txtpal(fun txtpal_c/1, List, [])
    end.

txtpal(F, [L|R], OL) ->
    L_1 = string:trim(binary_to_list(L)) ++ " ",
    case F(L_1) of
        false ->
            txtpal(F, R, OL);
        {_, _, _}=Col ->
            txtpal(F, R, [Col|OL])
    end;
txtpal(_, [], OL) ->
    {palette, lists:reverse(OL)}.

-define(ISALPHANUM(A), (
    (A >= $0 andalso A =< $9) orelse
    (A >= $A andalso A =< $Z) orelse
    (A >= $a andalso A =< $z))).

-define(HEXDIGIT(A), (
    (A >= $0 andalso A =< $9) orelse
    (A >= $A andalso A =< $F) orelse
    (A >= $a andalso A =< $f))).

-define(ISDIGIT(A), (
    (A >= $0 andalso A =< $9))).

-define(ISQUOTE(Q), (Q =:= 34 orelse Q =:= $')).

txt_hexv(A) when A >= $0, A =< $9 ->
    A - $0;
txt_hexv(A) when A >= $A, A =< $F ->
    A - $A + 10;
txt_hexv(A) when A >= $a, A =< $f ->
    A - $a + 10.

txt_strnum(Num) ->
    case string:to_integer(Num) of
        {Num_I, _} when is_integer(Num_I) -> Num_I;
        _ -> 0
    end.

txt_strflt(Num) ->
    case string:to_float(Num) of
        {Num_I, _} when is_float(Num_I) -> Num_I;
        _ -> 0.0
    end.

%% Freeform
txtpal_f([Q,$#,H1,H2,H3,H4,H5,H6,Q|_])
  when ?HEXDIGIT(H1), ?HEXDIGIT(H2),
       ?HEXDIGIT(H3), ?HEXDIGIT(H4),
       ?HEXDIGIT(H5), ?HEXDIGIT(H6),
       ?ISQUOTE(Q) ->
    R = (txt_hexv(H1) bsl 4) bor txt_hexv(H2),
    G = (txt_hexv(H3) bsl 4) bor txt_hexv(H4),
    B = (txt_hexv(H5) bsl 4) bor txt_hexv(H6),
    {R / 255.0,G / 255.0,B / 255.0};
txtpal_f([Q,H1,H2,H3,H4,H5,H6,Q|_])
  when ?HEXDIGIT(H1), ?HEXDIGIT(H2),
       ?HEXDIGIT(H3), ?HEXDIGIT(H4),
       ?HEXDIGIT(H5), ?HEXDIGIT(H6),
       ?ISQUOTE(Q) ->
    R = (txt_hexv(H1) bsl 4) bor txt_hexv(H2),
    G = (txt_hexv(H3) bsl 4) bor txt_hexv(H4),
    B = (txt_hexv(H5) bsl 4) bor txt_hexv(H6),
    {R / 255.0,G / 255.0,B / 255.0};
txtpal_f([Q,H1,H2,H3,Q|_])
  when ?HEXDIGIT(H1), ?HEXDIGIT(H2), ?HEXDIGIT(H3),
       ?ISQUOTE(Q) ->
    R = (txt_hexv(H1) bsl 4),
    G = (txt_hexv(H2) bsl 4),
    B = (txt_hexv(H3) bsl 4),
    {R / 255.0,G / 255.0,B / 255.0};
txtpal_f([$#,H1,H2,H3,H4,H5,H6,AC|_])
  when ?HEXDIGIT(H1), ?HEXDIGIT(H2),
       ?HEXDIGIT(H3), ?HEXDIGIT(H4),
       ?HEXDIGIT(H5), ?HEXDIGIT(H6),
       not ?ISALPHANUM(AC) ->
    R = (txt_hexv(H1) bsl 4) bor txt_hexv(H2),
    G = (txt_hexv(H3) bsl 4) bor txt_hexv(H4),
    B = (txt_hexv(H5) bsl 4) bor txt_hexv(H6),
    {R / 255.0,G / 255.0,B / 255.0};
txtpal_f([$#,H1,H2,H3,AC|_])
  when ?HEXDIGIT(H1), ?HEXDIGIT(H2), ?HEXDIGIT(H3),
       not ?ISALPHANUM(AC) ->
    R = (txt_hexv(H1) bsl 4),
    G = (txt_hexv(H2) bsl 4),
    B = (txt_hexv(H3) bsl 4),
    {R / 255.0,G / 255.0,B / 255.0};
txtpal_f("rgb(" ++ Str) ->
    case string:tokens(Str, " ,") of
        [[C1|_]=Num1,[C2|_]=Num2,[C3|_]=Num3|_]
          when ?ISDIGIT(C1), ?ISDIGIT(C2), ?ISDIGIT(C3) ->
            R = txt_strnum(Num1),
            G = txt_strnum(Num2),
            B = txt_strnum(Num3),
            {R / 255.0,G / 255.0,B / 255.0};
        _ ->
            txtpal_f(Str)
    end;
txtpal_f([_|R]) ->
    txtpal_f(R);
txtpal_f([]) ->
    false.

%% Columnar data
txtpal_c([$F,$F,H1,H2,H3,H4,H5,H6,AC|_]) %% ARGB hex code
  when ?HEXDIGIT(H1), ?HEXDIGIT(H2),
       ?HEXDIGIT(H3), ?HEXDIGIT(H4),
       ?HEXDIGIT(H5), ?HEXDIGIT(H6),
       not ?ISALPHANUM(AC) ->
    R = (txt_hexv(H1) bsl 4) bor txt_hexv(H2),
    G = (txt_hexv(H3) bsl 4) bor txt_hexv(H4),
    B = (txt_hexv(H5) bsl 4) bor txt_hexv(H6),
    {R / 255.0,G / 255.0,B / 255.0};
txtpal_c([H1,H2,H3,H4,H5,H6,AC|_])
  when ?HEXDIGIT(H1), ?HEXDIGIT(H2),
       ?HEXDIGIT(H3), ?HEXDIGIT(H4),
       ?HEXDIGIT(H5), ?HEXDIGIT(H6),
       not ?ISALPHANUM(AC) ->
    R = (txt_hexv(H1) bsl 4) bor txt_hexv(H2),
    G = (txt_hexv(H3) bsl 4) bor txt_hexv(H4),
    B = (txt_hexv(H5) bsl 4) bor txt_hexv(H6),
    {R / 255.0,G / 255.0,B / 255.0};
txtpal_c(Str) ->
    case string:tokens(Str, " ,\t") of
        [[C1,$.|_]=Num1,[C2,$.|_]=Num2,[C3,$.|_]=Num3|_]
          when ?ISDIGIT(C1), ?ISDIGIT(C2), ?ISDIGIT(C3) ->
            R = txt_strflt(Num1),
            G = txt_strflt(Num2),
            B = txt_strflt(Num3),
            {R,G,B};
        [[C1|_]=Num1,[C2|_]=Num2,[C3|_]=Num3|_]
          when ?ISDIGIT(C1), ?ISDIGIT(C2), ?ISDIGIT(C3) ->
            R = txt_strnum(Num1),
            G = txt_strnum(Num2),
            B = txt_strnum(Num3),
            {R / 255.0,G / 255.0,B / 255.0};
        _ ->
            false
    end.

%%%
%%%

%% Information for many palette formats:
%% http://www.selapa.net/swatches/colors/fileformats.php
%%

%%%
%%%

read_act(Filename) ->
    case file:read_file(Filename) of
        {ok, Cont} ->
            act_entry(Cont, 0, [])
    end.

act_entry(<<R,G,B,Next/binary>>, I, OL) when I < 256 ->
    act_entry(Next, I+1, [{R / 255.0, G / 255.0, B / 255.0}|OL]);
act_entry(_, I, OL) when I >= 256 ->
    {palette, lists:reverse(OL)}.


%%%
%%%

%%% Adobe color

-define(UINTBE, big-unsigned-integer).
-define(SINTBE, big-signed-integer).

read_aco(Filename) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            aco(Content)
    end.

aco(<<Version:16/?UINTBE,Rest/binary>>) ->
    case Version of
        0 ->
            aco_v1(Rest);
        1 ->
            aco_v1(Rest);
        2 ->
            aco_v2(Rest);
        _ ->
            {error, unsupported}
    end.

aco_v1(<<Count:16/?UINTBE, Rest/binary>>) ->
    aco_v1_item(0, Count, Rest, []).
    
aco_v1_item(I, Count, <<CSID:16/?UINTBE,ColDat:8/binary-unit:8,Rest/binary>>, OL)
  when I < Count ->
    I_1 = I + 1,
    case aco_item_1(CSID, ColDat) of
        {_, _, _}=Col ->
            aco_v1_item(I_1, Count, Rest, [Col|OL])
    end;
aco_v1_item(I, Count, _, OL)
  when I >= Count ->
    {palette, lists:reverse(OL)}.

aco_v2(<<Count:16/?UINTBE, Rest/binary>>) ->
    aco_v2_item(0, Count, Rest, []).
    
aco_v2_item(I, Count, <<CSID:16/?UINTBE,ColDat:8/binary-unit:8,Rest/binary>>, OL)
  when I < Count ->
    I_1 = I + 1,
    case aco_item_1(CSID, ColDat) of
        {_, _, _}=Col ->
            Rest_1 = aco_v2_name(Rest),
            aco_v2_item(I_1, Count, Rest_1, [Col|OL])
    end;
aco_v2_item(I, Count, _, OL)
  when I >= Count ->
    {palette, lists:reverse(OL)}.
aco_v2_name(<<0:16/?UINTBE,Size:16/?UINTBE,Rest/binary>>) ->
    Size1 = (Size-1) bsl 1,
    <<0:16/?UINTBE,Rest_1/binary>> = binary:part(Rest, {Size1, byte_size(Rest) - Size1}),
    Rest_1.


-spec aco_item_1(integer(), binary()) -> {float(),float(),float()}.

%% RGB
aco_item_1(0, <<R:16/?UINTBE,G:16/?UINTBE,B:16/?UINTBE,_:16>>) ->
    {R / 65535.0, G / 65535.0, B / 65535.0};

%% HSB
aco_item_1(1, <<H_0:16/?UINTBE,S_0:16/?UINTBE,B_0:16/?UINTBE,_:16>>) ->
    H_1 = H_0 / 65535.0 * 360, % hue
    S_1 = S_0 / 65535.0, % saturation
    B_1 = B_0 / 65535.0, % brightness
    {R,G,B} = wings_color:hsb_to_rgb(H_1, S_1, B_1),
    {R,G,B};

%% CMYK
aco_item_1(2, <<C:16/?UINTBE,M:16/?UINTBE,Y:16/?UINTBE,K:16/?UINTBE>>) ->
    {R,G,B} = wings_color:cmyk_to_rgb(
        1.0 - (C / 65535.0),
        1.0 - (M / 65535.0),
        1.0 - (Y / 65535.0),
        1.0 - (K / 65535.0)),
    {R,G,B};

%% LAB
aco_item_1(7, <<L_0:16/?UINTBE,C_A_0:16/?SINTBE,C_B_0:16/?SINTBE,_:16>>) ->
    L = L_0 / 100.0, % 0 to 10000
    C_A = C_A_0 / 100.0, % -12800 to 12700
    C_B = C_B_0 / 100.0, % -12800 to 12700
    wings_color:lab_to_rgb(L, C_A, C_B);

%% Grayscale
aco_item_1(8, <<Val:16/?UINTBE,_/binary>>) ->
    Val_1 = Val / 10000.0,
    {Val_1, Val_1, Val_1};

%% Also CMYK
aco_item_1(9, <<C:16/?UINTBE,M:16/?UINTBE,Y:16/?UINTBE,K:16/?UINTBE>>) ->
    {R,G,B} = wings_color:cmyk_to_rgb(C / 10000.0, M / 10000.0, Y / 10000.0, K / 10000.0),
    {R,G,B};

aco_item_1(_, _) ->
    {0.0,0.0,0.0}.

%%%
%%%

-define(BFLT, big-float).

read_ase(Filename) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            read_ase_1(Content)
    end.

read_ase_1(<<"ASEF", Ver1:16/?UINTBE, Ver2:16/?UINTBE, NumBlocks:32/?UINTBE, R/binary>>)
  when Ver1 =:= 1, Ver2 =:= 0 ->
    ase_block(R, [], NumBlocks).

ase_block(_, Cols, 0) ->
    {palette, lists:reverse(Cols)};
ase_block(<<BlockType:16/?UINTBE,Size:32/?UINTBE,R/binary>>, Cols, NumBlocks) ->
    case BlockType of
        16#0001 ->
            BinThis = binary:part(R, {0, Size}),
            C = ase_item(BinThis),
            BinNext = binary:part(R, {Size, byte_size(R)-Size}),
            ase_block(BinNext, [C|Cols], NumBlocks-1);
        _       ->
            R_1 = binary:part(R, {Size, byte_size(R)-Size}),
            ase_block(R_1, Cols, NumBlocks-1)
    end;
ase_block(<<>>, Cols, _) ->
    {palette, Cols}.

ase_item(<<SLen:16/?UINTBE,Bin/binary>>) ->
    SLen_2 = SLen bsl 1,
    After = binary:part(Bin, {SLen_2, byte_size(Bin)-SLen_2}),
    case After of
        <<"CMYK",C:32/?BFLT,M:32/?BFLT,Y:32/?BFLT,K:32/?BFLT,_/binary>> ->
            {R,G,B} = wings_color:cmyk_to_rgb(C,M,Y,K),
            {R,G,B};
        <<"RGB ",R:32/?BFLT,G:32/?BFLT,B:32/?BFLT,_/binary>> ->
            {R,G,B};
        <<"LAB ",L_0:32/?BFLT,A_0:32/?BFLT,B_0:32/?BFLT,_/binary>> ->
            {R,G,B} = wings_color:lab_to_rgb(L_0 * 100, A_0, B_0),
            {R,G,B};
        <<"Gray",V:32/?BFLT,_/binary>> ->
            {V,V,V}
    end.


%%%
%%%

%%% Adobe color book

read_acb(Filename) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            acb(Content)
    end.    

get_string(<<Len_0:32/?UINTBE,R/binary>>) ->
    Len = Len_0 bsl 1,
    Str = binary:part(R, 0, Len),
    R1 = binary:part(R, Len, byte_size(R) - Len),
    {Str, R1}.
 

acb(<<"8BCB", Version:16/?UINTBE, _CBID:16/?UINTBE, Rest/binary>>) ->
    case Version =:= 1 of
        true ->
            acb_1(Rest)
    end.

acb_1(R1) ->
    {_Title, R2} = get_string(R1),
    {_Prefix, R3} = get_string(R2),
    {_Postfix, R4} = get_string(R3),
    {_Desc, R5} = get_string(R4),
    << ColCount:16/?UINTBE,
       _Unused1:16/?UINTBE,
       _Unused2:16/?UINTBE,
       ColSpc:16/?UINTBE,
       R6/binary >> = R5,
    acb_color(0, ColCount, R6, ColSpc, []).

acb_color(I, Count, R1, Type, OL)
  when I < Count ->
    {_Name, R2} = get_string(R1),
    <<_Code:6/binary-unit:8, R3/binary>> = R2,
    {Color, R4} = acb_color_1(Type, R3),
    acb_color(I+1, Count, R4, Type, [Color|OL]);
acb_color(I, Count, _, _, OL)
  when I >= Count ->
    {palette, lists:reverse(OL)}.

acb_color_1(0, <<R,G,B,Next/binary>>) ->
    {{R / 255.0,G / 255.0,B / 255.0}, Next};

acb_color_1(2, <<C,M,Y,K_0,Next/binary>>) ->
    {R,G,B} = wings_color:cmyk_to_rgb(
        1.0 - (C / 255.0),
        1.0 - (M / 255.0),
        1.0 - (Y / 255.0),
        1.0 - (K_0 / 255.0)),
    {{R,G,B}, Next};

acb_color_1(7, <<L_0,C_A_0,C_B_0,Next/binary>>) ->
    L = L_0 / 255.0 * 100.0, %% change 0..255 to 0..100
    C_A = C_A_0 - 128,
    C_B = C_B_0 - 128,
    {wings_color:lab_to_rgb(L, C_A, C_B), Next}.


%%%
%%%

remove_none(A) ->
    A1 = lists:dropwhile(
            fun (C) -> C =:= none end,
            lists:reverse(A)),
    lists:reverse(A1).

remove_all_none(A0) ->
    A1 = remove_none(A0),
    [C || C <- A1, C =/= none].

%%%
%%%

read_samp_file(Filename) ->
    case load_image(Filename) of
        #e3d_image{}=Img ->
            read_samp_img(Img)
    end.
read_samp_img(#e3d_image{image=IB,type=Type}) ->
    case Type of
        r8g8b8 ->
            read_samp_img_1(IB);
        r8g8b8a8 ->
            IB1 = rgba_to_rgb(IB),
            read_samp_img_1(IB1)
    end.
read_samp_img_1(Cont) ->
    Palette = [rgb_to_1(Col) || {Col,_} <- pop(Cont)],
    {palette, Palette}.

    

rgba_to_rgb(Bin) ->
    rgba_to_rgb(Bin, []).
rgba_to_rgb(<<>>, OL) ->
    iolist_to_binary(lists:reverse(OL));
rgba_to_rgb(<<R,G,B,_,Bin/binary>>, OL) ->
    rgba_to_rgb(Bin, [<<R,G,B>>|OL]).


    
rgb_to_1({R,G,B}) ->
    {R / 255.0, G / 255.0, B / 255.0}.

%% Sample the colors of a bitmap image and get a palette with colors
%% sorted by most used to least used, and then shrink the resulting
%% palette by trimming out similar colors until the palette size is
%% smaller than 256.
%% 
pop(Cont) ->
    pop(Cont, maps:new()).
pop(<<R,G,B,Cont/binary>>, Counts) ->
    Color = {R,G,B},
    Pv = maps:get(Color, Counts, 0),
    pop(Cont, Counts#{Color => Pv+1});
pop(<<>>, Counts) ->
    List = lists:usort(fun pop_sort_col/2, maps:to_list(Counts)),
    pop_1(lists:reverse(List), 4).
pop_1(List, Amt) when Amt < 8 ->
    List2 = pop_2(List, Amt, 256),
    if length(List2) > 256 ->
            %% Re-sort after the merged colors
            List3 = lists:usort(fun pop_sort_col/2, List2),
            pop_1(lists:reverse(List3), Amt+2);
        true ->
            List2
    end;
pop_1(List, Amt) when Amt >= 8 ->
    %% The color similarity stemming is limited to a difference
    %% of 8 intensity levels, if we still have a lot of colors,
    %% at this point, just trim out the less used colors entirely.
    List2 = pop_2(List, Amt, 256),
    lists:sublist(List2, 1, 256).
pop_sort_col({_, C1}, {_, C2}) -> C1 < C2.

pop_2(List, _Amt, 0) ->
    List;
pop_2([Col1|List1], Amt, Max) when length(List1)+1 > Max ->
    %% Get similar colors
    Fun = fun (C2) -> pop_col_similar(Col1, C2, Amt) end,
    {SimilarL, List2} = lists:partition(Fun, List1),
    [pop_merge_cols([Col1|SimilarL])|pop_2(List2, Amt, Max-1)];
pop_2(List, _Amt, Max) when length(List) =< Max ->
    List.

%% Merge together a list of similar colors into a single color
pop_merge_cols([{{CR,CG,CB},CCo}|List]) ->
    pop_merge_cols(List, CR, CG, CB, CCo).
pop_merge_cols([{{CR,CG,CB},CCo}|List],R0,G0,B0,Co) ->
    Total_I = Co + CCo,
    Total = float(Total_I),
    Co_1 = Co / Total,
    CCo_1 = CCo / Total,
    R1 = (R0 * Co_1) + (CR * CCo_1),
    G1 = (G0 * Co_1) + (CG * CCo_1),
    B1 = (B0 * Co_1) + (CB * CCo_1),
    pop_merge_cols(List,R1,G1,B1,Total_I);
pop_merge_cols([],R1,G1,B1,Count) ->
    {{round(R1),round(G1),round(B1)},Count}.

%% Within range between channel intensities
-define(WITHIN_AMT(V1,V2,Amt),
    ((((V2-Amt) =< V1) andalso (V1 =< (V2+Amt))) orelse
     (((V1-Amt) =< V2) andalso (V2 =< (V1+Amt)))) ).

pop_col_similar({{R1,G1,B1}, _}, {{R2,G2,B2}, _}, Amt)
    when ?WITHIN_AMT(R1,R2,Amt),
         ?WITHIN_AMT(G1,G2,Amt),
         ?WITHIN_AMT(B1,B2,Amt) ->
    true;
pop_col_similar(_, _, _Amt) ->
    false.

%%%
%%%

load_image(Filename) ->
    case string:to_lower(filename:extension(Filename)) of
        ".jpeg" -> Ext = jpg;
        ".jpg"  -> Ext = jpg;
        _       -> Ext = e3d
    end,
    case Ext of
        jpg ->
            F = fun load_image_jpeg/1;
        _ ->
            F = fun load_image_e3d/1
    end,
    F(Filename).

load_image_e3d(Filename) ->
    e3d_image:load(Filename).

load_image_jpeg(Filename) ->
    BlockWxMsgs = wxLogNull:new(),
    Ret = load_image_jpeg_1(Filename),
    wxLogNull:destroy(BlockWxMsgs),
    Ret.
load_image_jpeg_1(Filename) ->
    Image = wxImage:new(),
    case wxImage:loadFile(Image, Filename) of
        true ->
            E3d = wings_image:wxImage_to_e3d(Image),
            wxImage:destroy(Image),
            e3d_image:fix_outtype(Filename, E3d, []);
        false ->
            {error, none}
    end.

%%%
%%%

export_filename(Ps0, Fun) ->
    Dir = wings_pref:get_value(current_directory),
    Ps = Ps0 ++ [{directory,Dir}],
    wings_plugin:call_ui({file,save_dialog,Ps,Fun}).

export(_St, swimg_svg=Kind, Pal0) ->
    empty_export_check(Pal0, fun() ->
        Ps = [{extensions,[{".svg",?__(1,"SVG Document")}]},
              {title,?__(2,"Export palette as SVG swatch image")}],
        export_filename(Ps, do_export_swimg(Kind, Pal0))
    end);
export(_St, swimg_eps=Kind, Pal0) ->
    empty_export_check(Pal0, fun() ->
        Ps = [{extensions,[{".eps",?__(3,"EPS Document")}]},
              {title,?__(4,"Export palette as EPS swatch image")}],
        export_filename(Ps, do_export_swimg(Kind, Pal0))
    end);
export(_St, text_columns=Kind, Pal0) ->
    empty_export_check(Pal0, fun() ->
        Ps = [{extensions,[{".txt",?__(5,"Plain text")}]},
              {title,?__(6,"Export palette as plain text (columns)")}],
        export_filename(Ps, do_export(Kind, Pal0))
    end);
export(_St, text_hex=Kind, Pal0) ->
    empty_export_check(Pal0, fun() ->
        Ps = [{extensions,[{".txt",?__(7,"Plain text")}]},
              {title,?__(8,"Export palette as plain text (hex)")}],
        export_filename(Ps, do_export(Kind, Pal0))
    end);
export(_St, legend_svg=Kind, Pal0) ->
    empty_export_check(Pal0, fun() ->
        Ps = [{extensions,[{".svg",?__(9,"SVG Document")}]},
              {title,?__(10,"Export palette as SVG legend")}],
        export_filename(Ps, do_export(Kind, Pal0))
    end);
export(_St, legend_tex=Kind, Pal0) ->
    empty_export_check(Pal0, fun() ->
        Ps = [{extensions,[{".tex",?__(11,"TeX Document")}]},
              {title,?__(12,"Export palette as TeX+TikZ legend")}],
        export_filename(Ps, do_export(Kind, Pal0))
    end).


do_export_swimg(Kind, Pal0) ->
    Kind1 = case Kind of
        swimg_svg -> svg;
        swimg_eps -> eps
    end,
    fun(Filename) ->
        try export_swimg(Kind1, remove_all_none(Pal0), Filename) of
            _ ->
                keep
        catch _:E:StT ->
            io:format("~w: Error ~p stack trace: ~p~n", [?MODULE,E,StT]),
            wpa:error_msg(?__(3,"An error occurred while trying to export.")),
            keep
        end
    end.

do_export(Kind, Pal0) ->
    Fun = case Kind of
        text_columns -> fun export_text_columns/2;
        text_hex -> fun export_text_hex/2;
        legend_svg -> fun export_legend_svg/2;
        legend_tex -> fun export_legend_tex/2
    end,
    fun(Filename) ->
        try Fun(remove_all_none(Pal0), Filename) of
            _ ->
                keep
        catch _:E:StT ->
            io:format("~w: Error ~p stack trace: ~p~n", [?MODULE,E,StT]),
            wpa:error_msg(?__(3,"An error occurred while trying to export.")),
            keep
        end
    end.


empty_export_check([], _Fun) ->
    Msg = ?__(1, "The current palette had not been modified, which means the "
                 "exported file will be empty.\nThe palette will need to be "
                 "modified first."),
    wpa:error_msg(Msg),
    keep;
empty_export_check(_, Fun) ->
    Fun().


%% Export swatch image
export_swimg(svg, Pal, Filename) ->
    export_swimg_svg(Pal, Filename);
export_swimg(eps, Pal, Filename) ->
    export_swimg_eps(Pal, Filename).

%% SVG swatch image
export_swimg_svg(Pal, Filename) ->
    {ok, Fp} = file:open(Filename, [write,binary]),
    io:format(Fp, "<?xml version=\"1.0\" standalone=\"no\"?>\r\n", []),
    io:format(Fp, "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"", []),
    io:format(Fp, " \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\r\n", []),
    io:format(Fp, "<svg xmlns=\"http://www.w3.org/2000/svg\"\r\n", []),
    io:format(Fp, " xmlns:xlink=\"http://www.w3.org/1999/xlink\"\r\n", []),
    io:format(Fp, " width=\"~wpx\" height=\"~wpx\"\r\n", [20*length(Pal), 20]),
    io:format(Fp, " viewBox=\"~w ~w ~w ~w\">\r\n", [0, 0, 20*length(Pal), 20]),
    export_swimg_svg_1(Fp, Pal, 0),
    io:format(Fp, "</svg>\r\n", []),
    file:close(Fp).
export_swimg_svg_1(Fp, [{R,G,B}|Pal1], I) ->
    R1 = round(R * 255),
    G1 = round(G * 255),
    B1 = round(B * 255),
    io:format(Fp, "<rect x=\"~w\" y=\"~w\" width=\"~w\" height=\"~w\" ", [I*20, 0, 20, 20]),
    io:format(Fp, "style=\"fill:~s;\"/>\r\n", [hex_code(R1,G1,B1)]),
    export_swimg_svg_1(Fp, Pal1, I+1);
export_swimg_svg_1(_, [], _I) ->
    ok.


%% EPS swatch image
export_swimg_eps(Pal, Filename) ->
    {ok, Fp} = file:open(Filename, [write,binary]),

    {{Y,Mo,D},{H,M,S}} = calendar:local_time(),
    
    ExpDate1 = lists:flatten(io_lib:format(
        "~2..0w", [Mo])),
    ExpDate2 = lists:flatten(io_lib:format(
        "~2..0w ~2..0w:~2..0w:~2..0w ~4..0w", [D,H,M,S,Y])),

    io:format(Fp, "%!PS-Adobe-3.0 EPSF-3.0\r\n", []),
    io:format(Fp, "%%Creator: Wings3D Palette Swatch Export\r\n", []),
    io:format(Fp, "%%CreationDate: ~s ~s\r\n", [ExpDate1, ExpDate2]),
    io:format(Fp, "%%Pages: 1\r\n", []),
    io:format(Fp, "%%BoundingBox: 0 0 ~w ~w\r\n", [20*length(Pal), 20]),
    io:format(Fp, "%%EndComments\r\n", []),
    io:format(Fp, "%%BeginProlog\r\n", []),
    io:format(Fp, "/cp {closepath} bind def\r\n", []),
    io:format(Fp, "/ef {eofill} bind def\r\n", []),
    io:format(Fp, "/l {lineto} bind def\r\n", []),
    io:format(Fp, "/m {moveto} bind def\r\n", []),
    io:format(Fp, "/np {newpath} bind def\r\n", []),
    io:format(Fp, "/srgb {setrgbcolor} bind def\r\n", []),
    io:format(Fp, "%%EndProlog\r\n", []),
    io:format(Fp, "%%Page: 1 1\r\n", []),
    export_swimg_eps_1(Fp, Pal, 0),
    io:format(Fp, "showpage\r\n", []),
    io:format(Fp, "%%Trailer\r\n", []),
    io:format(Fp, "%EOF\r\n", []),
    file:close(Fp).
export_swimg_eps_1(Fp, [{R,G,B}|Pal1], I) ->
    X1 = I*20,
    Y1 = 0,
    X2 = X1+20,
    Y2 = Y1+20,
    io:format(Fp, "np ~w ~w m ~w ~w l ~w ~w l ~w ~w l", [X1,Y1,X1,Y2,X2,Y2,X2,Y1]),
    io:format(Fp, " cp ~w ~w ~w srgb ef \r\n", [R,G,B]),
    export_swimg_eps_1(Fp, Pal1, I+1);
export_swimg_eps_1(_, [], _I) ->
    ok.


%% SVG Legend
export_legend_svg(Pal, Filename) ->
    M = 20, %% Margin
    T = 10, %% Between items
    {ok, Fp} = file:open(Filename, [write,binary]),
    io:format(Fp, "<?xml version=\"1.0\" standalone=\"no\"?>\r\n", []),
    io:format(Fp, "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"", []),
    io:format(Fp, " \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\r\n", []),
    io:format(Fp, "<svg xmlns=\"http://www.w3.org/2000/svg\"\r\n", []),
    io:format(Fp, " xmlns:xlink=\"http://www.w3.org/1999/xlink\"\r\n", []),
    io:format(Fp, " width=\"~wpx\" height=\"~wpx\"\r\n", [300, (T+20)*length(Pal)+M+M]),
    io:format(Fp, " viewBox=\"~w ~w ~w ~w\">\r\n", [0, 0, 300, (T+20)*length(Pal)+M+M]),
    io:format(Fp, "<g>\r\n", []),
    io:format(Fp, "<rect x=\"0\" y=\"0\" width=\"~w\" height=\"~w\"", [300, (T+20)*length(Pal)+M+M]),
    io:format(Fp, " style=\"stroke:black;stroke-width:1px;fill:white\"/>\r\n", []),
    export_legend_svg_1(Fp, Pal, 0, M, T),
    io:format(Fp, "</g>\r\n", []),
    io:format(Fp, "</svg>\r\n", []),
    file:close(Fp).
export_legend_svg_1(Fp, [{R,G,B}|Pal1], I, M, T) ->
    R1 = round(R * 255),
    G1 = round(G * 255),
    B1 = round(B * 255),

    ColorText = io_lib:format("Color~w", [I+1]),
    io:format(Fp, "<rect x=\"~w\" y=\"~w\" width=\"~w\" height=\"~w\"", [M, M+(T+20)*I, 20, 20]),
    io:format(Fp, " style=\"fill:~s;\"/>\r\n", [hex_code(R1,G1,B1)]),
    io:format(Fp, "<text xml:space=\"preserve\" x=\"~w\" y=\"~w\"", [M+20+T, M+15+(T+20)*I]),
    io:format(Fp, " font-family=\"sans-serif\" font-style=\"normal\"", []),
    io:format(Fp, " font-weight=\"normal\" font-size=\"10\" text-anchor=\"start\">", []),
    io:format(Fp, "~s", [ColorText]),
    io:format(Fp, "</text>\r\n", []),
    export_legend_svg_1(Fp, Pal1, I+1, M, T);
export_legend_svg_1(_, [], _I, _M, _T) ->
    ok.
    

%% TeX+TiKz Legend
export_legend_tex(Pal, Filename) ->
    {ok, Fp} = file:open(Filename, [write,binary]),
    io:format(Fp, "\\documentclass{article}\r\n", []),
    io:format(Fp, "\\usepackage{tikz}\r\n", []),
    io:format(Fp, "\\begin{document}\r\n", []),
    io:format(Fp, "\\begin{tikzpicture}\r\n", []),
    io:format(Fp, "\\matrix [draw=black]\r\n", []),
    io:format(Fp, "  {\r\n", []),
    export_legend_tex_1(Fp, Pal, 0),
    io:format(Fp, "  };\r\n", []),
    io:format(Fp, "\\end{tikzpicture}\r\n", []),
    io:format(Fp, "\\end{document}\r\n", []),
    file:close(Fp).
export_legend_tex_1(Fp, [{R,G,B}|Pal1], I) ->
    R1 = round(R * 255),
    G1 = round(G * 255),
    B1 = round(B * 255),
    ColorText = io_lib:format("Color~w", [I+1]),
    io:format(Fp, "    \\fill [fill={rgb,255:red,~w; green,~w; blue,~w}] ", [R1,G1,B1]),
    io:format(Fp, "(0.0,-0.2) rectangle (0.4,0.2); & \\node {~s}; \\\\\r\n", [ColorText]),
    export_legend_tex_1(Fp, Pal1, I+1);
export_legend_tex_1(_, [], _I) ->
    ok.



%% Text file (Columns)
export_text_columns(Pal, Filename) ->
    {ok, Fp} = file:open(Filename, [write,binary]),
    export_text_columns_1(Fp, Pal, 1),
    file:close(Fp).
export_text_columns_1(Fp, [{R,G,B}|Pal1], I) ->
    R1 = round(R * 255),
    G1 = round(G * 255),
    B1 = round(B * 255),
    io:format(Fp, "~w  ~w  ~w  Color~w\r\n", [R1, G1, B1, I]),
    export_text_columns_1(Fp, Pal1, I+1);
export_text_columns_1(_, [], _I) ->
    ok.

    
%% Text file (Hex)
export_text_hex(Pal, Filename) ->
    {ok, Fp} = file:open(Filename, [write,binary]),
    export_text_hex_1(Fp, Pal),
    file:close(Fp).
export_text_hex_1(Fp, [{R,G,B}|Pal1]) ->
    R1 = round(R * 255),
    G1 = round(G * 255),
    B1 = round(B * 255),
    io:format(Fp, "~s\r\n", [hex_code(R1,G1,B1)]),
    export_text_hex_1(Fp, Pal1);
export_text_hex_1(_, []) ->
    ok.


hex_digit(Num) when Num >= 0 andalso Num < 10 -> $0 + Num;
hex_digit(Num) when Num >= 10 andalso Num < 16 -> $a + (Num-10).
two_hex(Num) ->
    [hex_digit((Num band 16#F0) bsr 4), hex_digit(Num band 16#0F)].
hex_code(R, G, B) ->
    "#" ++ two_hex(R) ++ two_hex(G) ++ two_hex(B).

