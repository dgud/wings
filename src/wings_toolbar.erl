%%
%%  wings_toolbar.erl --
%%
%%     Toolbar for geometry and AutoUV windows.
%%
%%  Copyright (c) 2004-2016 Bjorn Gustavsson & Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_toolbar).

-export([init/2, update/2]).

-include("wings.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Toolbar

update({selmode, Mode, Sh}, #{mode:=Mode, sh:=Sh}=TB) ->
    TB;
update({selmode, Mode, Sh}, #{me:=TB, restr:=Restr}=St) ->
    update_selection(TB, Mode, Sh, Restr),
    St#{mode:=Mode, sh:=Sh};
update({mode_restriction, Restr}, #{me:=TB, mode:=Mode, sh:=Sh}=St) ->
    update_selection(TB, Mode, Sh, Restr),
    St#{restr:=Restr};
update({view, Key0, State}, #{me:=TB}=St) ->
    {Key, Value} = case Key0 of
		       {show, K} -> {K, State};
		       workmode -> {workmode, not State};
		       _ -> {Key0, State}
		   end,
    case wings_menu:predefined_item(toolbar, Key) of
	false -> ignore;
	Id -> wxToolBar:toggleTool(TB, Id, Value)
    end,
    St.

update_selection(TB, Mode, Sh, Restr) ->
    All = [vertex, edge, face, body],
    Enabled = button_restrict(All, Restr),
    States = [{Butt, wings_menu:predefined_item(toolbar, Butt),
	       Butt =:= Mode orelse button_sh_filter(Butt, Sh)}
	      || Butt <- All],
    [wxToolBar:enableTool(TB, Id, lists:member(Butt, Enabled)) || {Butt, Id, _} <- States],
    [wxToolBar:toggleTool(TB, Id, State) || {_, Id, State} <- States].

button_restrict(Buttons, none) -> Buttons;
button_restrict(_Buttons, Restr) -> Restr.

button_sh_filter(_, false) -> false;
button_sh_filter(vertex, true) -> true;
button_sh_filter(edge, true) -> true;
button_sh_filter(face, true) -> true;
button_sh_filter(_, _) -> false.

init(Frame, RawIcons) ->
    wxSystemOptions:setOption("msw.remap", 2),
    TB = wxFrame:createToolBar(Frame),
    Icons = binary_to_term(RawIcons),
    Bs = [make_bitmap(B, Icons) || B <- buttons()],
    Tools = case wings_pref:get_value(extended_toolbar) of
		true -> Bs;
		false -> standard(Bs)
	    end,
    [add_tools(Tool, TB) || Tool <- Tools],
    %% wxToolBar:connect(TB, left_down, [{skip, true}]),
    %% wxToolBar:connect(TB, left_up, [{skip, true}]),
    %% wxToolBar:connect(TB, comand_tool_rclicked, [{skip, true}]),
    %% wxToolBar:connect(TB, comand_tool_enter, [{skip, true}]),
    wxToolBar:realize(TB),
    State = #{me=>TB, mode=>vertex, sh=>true, restr=>none, bs=>Bs},
    update({selmode, face, true}, State).

buttons() ->
    [make_button(open, normal, "wxART_FILE_OPEN"),
     make_button(save, normal, "wxART_FILE_SAVE"),
     make_button(undo, normal, "wxART_UNDO"),
     make_button(redo, normal, "wxART_REDO"),
     separator,
     make_button(vertex, toggle),
     make_button(edge, toggle),
     make_button(face, toggle),
     make_button(body, toggle),
     separator,
     make_button(pref, normal),
     make_button(workmode,toggle), make_button(orthogonal_view, toggle),
     make_button(show_groundplane, toggle), make_button(show_axes, toggle)].

make_button(Name, Type) ->
    make_button(Name, Type, undefined).

make_button(Name, Type, Art) ->
    Id = wings_menu:predefined_item(toolbar, Name),
    true = is_integer(Id),
    #{name=>Name, id=>Id, art=>Art, type=>Type}.


standard(Bs) ->
    Standard = [workmode,orthogonal_view,separator,
		vertex,edge,face,body,separator,
		show_groundplane,show_axes],
    GetButton = fun(separator) -> separator;
		   (Name) -> wings_util:mapsfind(Name, name, Bs)
		end,
    [GetButton(Value) || Value <- Standard].

icon_name(workmode) -> smooth;
icon_name(orthogonal_view) -> perspective;
icon_name(show_groundplane) -> groundplane;
icon_name(show_axes) -> axes;
icon_name(Name) -> Name.

make_bitmap(#{name:=Name,art:=undefined}=B, RawIcons) ->
    {_,{Bpp, W, H, Bin0}} = lists:keyfind(icon_name(Name), 1, RawIcons),
    {Colors, Alpha} = setup_image(Bin0, Bpp, W),
    Image = wxImage:new(W,H,Colors, [{static_data, true}]),
    wxImage:setAlpha(Image, Alpha, [{static_data, true}]),
    Bm = wxBitmap:new(Image),
    true = wxBitmap:ok(Bm),
    wxImage:destroy(Image),
    B#{bm=>Bm};
make_bitmap(#{art:=Art}=B, RawIcons) ->
    BM = wxArtProvider:getBitmap(Art, [{client, "wxART_TOOLBAR"}]),
    case BM == ?wxNullBitmap of
	true -> %% Load our backup bitmap
	    make_bitmap(B#{art:=undefined}, RawIcons);
	false ->
	    B#{bm=>BM}
    end;
make_bitmap(separator, _RawIcons) -> separator.

add_tools(separator, TB) ->
    wxToolBar:addStretchableSpace(TB);
add_tools(#{id:=Id, bm:=BM, type:=Type, name:=Tool}, TB) ->
    Kind = button_kind(Type),
    Help = button_help(Tool),
    Opts = [{kind, Kind}, {shortHelp, Help}, {longHelp, Help}],
    wxToolBar:addTool(TB, Id, "", BM, ?wxNullBitmap, Opts),
    wxToolBar:enableTool(TB, Id, true),
    case Type of
	toggle when Tool =:= workmode ->
	    wxToolBar:toggleTool(TB, Id, not wings_pref:get_value(Tool, false));
	toggle ->
	    wxToolBar:toggleTool(TB, Id, wings_pref:get_value(Tool, false));
	_ ->
	    ignore
    end,
    ok.

button_kind(toggle) -> ?wxITEM_CHECK;
button_kind(normal) -> ?wxITEM_NORMAL.

%% FIXME when icons are fixed
%% Poor mans version of alpha channel
setup_image(Bin0, 3, Width) ->
    RowLen = 3*Width,
    Bin = iolist_to_binary(lists:reverse([Row || <<Row:RowLen/binary>> <= Bin0])),
    rgb3(Bin, <<>>, <<>>);
setup_image(Bin0, 4, Width) ->
    RowLen = 4*Width,
    Bin = iolist_to_binary(lists:reverse([Row || <<Row:RowLen/binary>> <= Bin0])),
    rgb4(Bin, <<>>, <<>>).

rgb3(<<8684676:24, Rest/binary>>, Cs, As) ->
    rgb3(Rest, <<Cs/binary, 8684676:24>>, <<As/binary, 0:8>>);
rgb3(<<R:8,G:8,B:8, Rest/binary>>, Cs, As) ->
    A0 = abs(R-132)/123,
    A1 = abs(G-132)/123,
    A2 = abs(B-132)/123,
    A = trunc(255*min(1.0, max(max(A0,A1),A2))),
    rgb3(Rest, <<Cs/binary, R:8, G:8, B:8>>, <<As/binary, A:8>>);
rgb3(<<>>, Cs, As) ->
    {Cs,As}.

rgb4(<<C:24, A:8, R/binary>>, Cs, As) ->
    rgb4(R, <<Cs/binary, C:24>>, <<As/binary, A:8>>);
rgb4(<<>>, Cs, As) ->
    {Cs,As}.

button_help(Tool) ->
    button_help_2(icon_name(Tool), undecided).

%button_help_2(vertex, vertex) -> ?__(1,"Select adjacent vertices");
button_help_2(vertex, _) ->  ?__(2,"Change to vertex selection mode");
%button_help_2(edge, edge) ->  ?__(3,"Select adjcacent edges");
button_help_2(edge, _) ->  ?__(4,"Change to edge selection mode");
%button_help_2(face, face) ->  ?__(5,"Select adjacent faces");
button_help_2(face, _) ->  ?__(6,"Change to face selection mode");
%button_help_2(body, body) -> "";
button_help_2(body, _) ->  ?__(7,"Change to body selection mode");
button_help_2(Button, _) -> button_help_3(Button).

button_help_3(groundplane) ->
    [hide(), ?__(6," or "), show(), " "|?STR(messages,groundplane,"ground plane")];
button_help_3(axes) ->
    [hide(), ?__(6," or "), show(), " "|?STR(messages,axes,"axes")];
button_help_3(perspective) ->
    [?STR(messages,change_between,"Change between")," ",
     ?STR(messages,orthogonal,"orthogonal view"), ?__(6," or "),
     ?STR(messages,perspective,"perspective view")];
button_help_3(smooth) ->
    [?STR(messages,show_objects,"Show objects with")," ",
     ?STR(messages,smooth,"smooth shading"), ?__(6," or "),
     ?STR(messages,flat,"flat shading")];

button_help_3(undo) -> ?__(1,"Undo the last command");
button_help_3(redo) -> ?__(2,"Redo the last command that was undone");
button_help_3(open) -> ?__(3,"Open a previously saved scene");
button_help_3(save) -> ?__(4,"Save the current scene");
button_help_3(pref) -> ?__(5,"Edit the preferences for Wings").

hide() -> ?STR(messages,hide,"Hide").
show() -> ?STR(messages,show,"Show").

