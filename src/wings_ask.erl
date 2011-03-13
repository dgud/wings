%%
%%  wings_ask.erl --
%%
%%     Dialog boxes.
%%
%%  Copyright (c) 2002-2011 Bjorn Gustavsson
%%	          2003-2006 Raimo Niskanen
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_ask).
-export([init/0,ask/3,ask/4,dialog/3,dialog/4,
	 hsv_to_rgb/1,hsv_to_rgb/3,rgb_to_hsv/1,rgb_to_hsv/3]).

-define(NEED_OPENGL, 1).
-define(NEED_ESDL, 1).
-include("wings.hrl").

-define(HMARGIN, 16).
-define(VMARGIN, 8).

-define(HFRAME_SPACING, (3*?CHAR_WIDTH div 2)).

-define(IS_SHIFTED(Mod), ((Mod) band ?SHIFT_BITS =/= 0)).
-define(BUTTON_MASK,
	(?SDL_BUTTON_LMASK bor ?SDL_BUTTON_MMASK bor ?SDL_BUTTON_RMASK)).

-import(lists, [reverse/1,reverse/2,duplicate/2,member/2,
		foldl/3,sum/1,duplicate/1]).


%%-define(DEBUG_CATEGORIES, [temp]).
-ifdef(DEBUG_CATEGORIES).
-define(DEBUG, true).
-endif.

-ifdef(DEBUG).
-ifndef(DEBUG_CATEGORIES).
-define(DEBUG_CATEGORIES, []). % [event,tree,other]
-endif.

-define(DEBUG_DISPLAY(Cat,X),
	case lists:member(Cat,?DEBUG_CATEGORIES) of
	    true -> io:format("~p~n", [{?MODULE,?LINE,(X)}]), true;
	    false -> true
	end).
-define(DEBUG_FORMAT(Cat,Fmt,Args),
	case lists:member(Cat,?DEBUG_CATEGORIES) of
	    true -> io:format(?MODULE_STRING":"++integer_to_list(?LINE)++" "
			      ++Fmt, Args);
	    false -> ok
	end).
-define(DMPTREE(Cat,Fi), 
	case lists:member(Cat,?DEBUG_CATEGORIES) of
	    true -> 
		io:format("~p~n", [{?MODULE,?LINE}]), 
		dmptree(Fi), 
		io:format("~p~n", [{?MODULE,?LINE}]);
	    false -> ok
	end).
-else.
-define(DEBUG_DISPLAY(_Cat,_X), true).
-define(DEBUG_FORMAT(_Cat,_Fmt,_Args), ok).
-define(DMPTREE(_Cat,Fi), ok).
-endif.



-record(s,
	{w,
	 h,
	 ox,
	 oy,
	 call,
	 focus,				%Field index
	 focusable,			%Tuple of field indexes
	 mouse_focus=false,		%Mouse hit the field
	 fi,				%Static data for all fields.
	 n,				%Number of fields.
	 coords,			%Coordinates for hit testing.
	 store,				%Data for all fields.
	 level,				%Levels of nesting.
	 owner,				%Where to send result.
	 grab_win			%Previously grabbed focus window.
	}).

%% Static data for every field.
-record(fi,
	{handler,			%Handler fun.
	 key=0,				%Field key.
	 index,				%Field index
	 state=inert,			%inert|disabled|enabled
	 minimized,			%true|false|undefined
	 hook,				%Field hook fun/2
	 flags=[],			%Flags field.
	 x,y,				%Upper left position.
	 w,h,				%Width, height.
	 stretch=0,			%Horizontal stretch koefficient
	 extra				%Container or leaf data
	}).

%% Extra static data for container fields
-record(container,
	{type,				%vframe|hframe|oframe
	 x,y,				%Container pos
	 w,h,				%Container size
	 fields,			%Contained fields, tuple()
	 selected,			%integer()|undefined
	 stretch,			%Sum of contained stretch
	 w0,h0				%Unpropagated outer (field) size
	}).

%% Extra static data for leaf fields
-record(leaf,
	{w,h				%Natural (min) size
	}).



init() ->
    init_history(),
    ok.

ask(Title, Qs, Fun) ->
    ask(true, Title, Qs, Fun).

ask(Bool, Title, Qs0, Fun) ->
    {Labels,Vals} = ask_unzip(Qs0),
    Qs = [{hframe,
	   [{vframe,Labels},
	    {vframe,Vals}]}],
    dialog(Bool, Title, Qs, Fun).

ask_unzip(Qs) ->
    ask_unzip(Qs, [], []).
ask_unzip([{Label,{menu,_,_}=Menu}|T], AccA, AccB) ->
    ask_unzip(T, [{label,Label}|AccA], [Menu|AccB]);
ask_unzip([{Label,{menu,_,_,_}=Menu}|T], AccA, AccB) ->
    ask_unzip(T, [{label,Label}|AccA], [Menu|AccB]);
ask_unzip([{Label,Def}|T], AccA, AccB) ->
    ask_unzip(T, [{label,Label}|AccA], [{text,Def}|AccB]);
ask_unzip([{Label,Def,Flags}|T], AccA, AccB) ->
    ask_unzip(T, [{label,Label}|AccA], [{text,Def,Flags}|AccB]);
ask_unzip([], Labels, Vals) ->
    {reverse(Labels),reverse(Vals)}.

%%
%% Syntax of Qs.
%% See also wpc_test_ask.erl for examples.
%%
%% Common types:
%%     String = string()
%%     Boolean = true | false
%%     Fields = [Field]
%%     Field  -- Any field tuple described below
%%     Key = term()
%%         Note: An integer() Key is a relative field address,
%%             any other term is an absolute storage identifier.
%%             Since an integer() Key mean something it is a bad idea
%%             to use a float() as Key. It should work, but the slightest
%%             programming mistake in this module may lead to a mismatch.
%%               Field common data (common for all field types, used for
%%             dialog tree traversing, etc) is stored in the dialog tree,
%%             consisting of nested #fi{} records.
%%               Field private data (special to a given field type), and 
%%             field values are all stored in the same gb_trees store.
%%               All fields have a field index in the dialog tree, which
%%             is an integer from 1 and up assigned in tree traversal
%%             order (depth first - same as Qs order).
%%               Field private data is stored with negative field index as 
%%             key in the gb_trees store and is different record types 
%%             depending on field type. Field values is stored in the 
%%             gb_trees store with the field index as key (the default),
%%             or if using an integer key, the field shares value with
%%             another field and the key is a relative field index
%%             offset. If the key is any other term it is the actual
%%             key used in the store.
%%               All fields that have a non-integer key will return
%%             {Key,Value} as the field result of the dialog. The
%%             others just returns Value. Most fields return values,
%%             except when noted below.
%%     Hook = function()
%%         Hook should have arity 2 and return 'void' for any 
%%         unknown arguments, like this:
%%         fun (is_disabled, {Var,I,Store}) -> Bool;
%%                 %% Called at redraw time for all fields.
%%                 %% For container fields the contained fields are
%%                 %% disabled but not the container field.
%%             (is_minimized, {Var,I,Store}) -> Bool;
%%                 %% Called at layout time for all fields
%%                 %% except Overlay frames.
%%             (update, {Var,I,Val,Store}) ->
%%                 keep|{store,NewStore}|done|{done,NewStore}|{layout,NewStore}
%%                 %% Should make sure Val gets stored in NewStore.
%%                 %% done|{done,_} finishes the dialog (may get restarted).
%%                 %% {layout,_} forces a layout pass.
%%                 %% Called when a field changes value.
%%             (menu_disabled, {Var,I,Store}) -> [Disabled];
%%                 %% Disabled = Val|{Val,MenuFlags}
%%                 %% Val = menu value
%%                 %% MenuFlags = menu flags prepended to other menu flags
%%                 %% Only called for menu fields.
%%             (_, _) -> void
%%         end
%%
%% Qs is either one field below (preferably containing more fields,
%% or a list of fields, in which case the list is enclosed in a
%% {hframe,...} containing two {vframe,...}s, the first containing
%% Qs and the second containing an Ok and a Cancel button.
%%
%%
%%
%% Container fields (dialog tree nodes):
%%
%% {hframe,Fields[,Flags]}			-- Horizontal frame
%% {vframe,Fields[,Flags]}			-- Vertical frame
%%     Flags = [Flag]
%%     Flag = {title,String}|{minimized,Boolean}|{key,Key}|{hook,Hook}|layout|
%%            checkbox|invert
%% Only frames with both 'title' and 'minimized' flags return a value.
%% The 'checkbox' flag changes style of the frame to have a checkbox
%% field in the header indicating the minimized state. The 'invert' 
%% flag makes the checkbox value and the return value of the field to 
%% be the inverted minimized state (the maximized state ;-).
%%
%% {oframe,Fields[,Flags]}			-- Overlay frame
%%     Flags = [Flag]
%%     Flag = {title,String}|{style,Style}|{key,Key}|{hook,Hook}|layout
%%     Style = menu|buttons  -- menu is default
%%
%%
%%
%% Composite fields (consisting of other fields)
%%
%% {vradio,Alts,DefaultValue[,Flags]}		-- Radio buttons vertically
%% {hradio,Alts,DefaultValue[,Flags]}		-- Radio buttons horizontally
%%     Alts = [{PromptString,Value}]
%%     Flags = [Flag]
%%     Flag = {key,Key}|{title,String}|{hook,Hook}
%% Example: see wpc_am.erl. These are {key,...} or {key_alt,...} fields 
%% in a {hframe,...} or {vframe,...}.
%%
%% {label_column,Rows}				-- Column of labeled fields
%%     Rows = [Row]
%%     Row = {String,Field}
%% This is a {hframe,...} containing one {vframe,...} with {label,String}
%% fields and one {vframe,Fields} containing the fields.
%%
%% {slider,{text,Def,Flags}}			-- Slider on text field
%% {slider,{color,Col,Flags}}			-- Slider on color box
%% See slider regular field below. This is a {hframe,...} containing
%% two fields.
%%
%% {button,{text,Def,Flags}}                    -- Text field with a 
%%                                                 Browse button
%%    Flags = [Flag]
%%    Flag = {props,DialogBoxProperties}|{dialog_type,DialogType}|
%%           {drop_flags,DropFlags}|RegularFlags
%%    DialogType = open_dialog|save_dialog
%%    DropFlags = [{index,Index}|{key,Key}|{hook,Hook}]
%%                %% Specifies field where any data dropped on the
%%                %% button will be stored
%%    Index = integer() relative field index to drop target
%%    Key = drop target field key
%%    Hook = drop target field hook
%% 
%% Regular fields (dialog tree leafs).
%% Additional types:
%%     RegularFlags = [RegularFlag]
%%     RegularFlag = {key,Key}|{hook,Hook}|{info,String}|
%%                   {stretch,Stretch}|layout
%% Note: the 'layout' flag forces a layout pass if the field changes value.
%%       Default Stretch is 0 and defines how much of horizontal padding
%%       when contained in a hframe that the field will use. The horizontal
%%       padding is distributed evenly over the stretch factors.
%%
%% panel					-- Blank filler, stretch 1
%% {panel,RegularFlags}
%% Does not return a value.
%%
%% {value,Value}				-- Invisible value holder
%% {value,Value,RegularFlags}
%%
%% {label,String}				-- Textual label
%% Does not return a value.
%%
%% separator					-- Horizontal separator line
%% Does not return a value.
%%
%% {color,Col[,RegularFlags]}			-- Color box with sub-dialog
%%     Col = {R,G,B}|{R,G,B,A}
%%     R = G = B = A = X, 0.0 =< X, X =< 1.0
%%
%% {alt,Def,String,Val[,RegularFlags]}		-- Radiobutton
%%     Def = term()  -- Start value for radiobutton group. 
%%                      One radiobutton in group must have this value.
%%     Val = term()  -- This button's value.
%%
%% {key_alt,{Key,Def},String,Val[,Flags]}	-- Radiobutton
%%    -> {alt,Def,String,Val,[{key,Key}|Flags]}
%%
%% {menu,Alts,Def[,RegularFlags]}		-- Pop-up menu
%%     Def = term()        -- Start value for menu.
%%                            One menu alternative must have this value.
%%     Alts = [Alt]
%%     Alt = {String,Val}|{String,Val,MenuFlags} -- Menu alt desc and value.
%%     Val = term()
%%     MenuFlags = [{info,String}]
%%
%% {button[,String],Action[,RegularFlags]}	-- Button
%%     Action = ok|cancel|done|function(Result)
%% Only Action == done returns a value.
%% If String is omitted and Action is an atom, a String is
%% constructed by capitalizing the atom's string representation. 
%% If Action is not an atom, String is mandatory.
%%
%% {custom,W,H,DrawFun[,RegularFlags]}		-- Custom look viewer
%%     W = H = integer()  -- Field size
%%     DrawFun = function(X, Y, W, H, Store)
%%         %% Should draw field and probably return 'keep'.
%%         %% Other return values are possible - read the source.
%%
%% {slider,{text,Def,Flags}}			-- Slider on text field
%% {slider,{color,Col,Flags}}			-- Slider on color box
%% {slider,Flags}				-- Solo slider
%%     Flags = [Flag]
%%     Flag = {range,{Min,Max}}|{color,true}|{color,ColKeySpec}|
%%            value|RegularFlag
%%     ColKeySpec = {r,KeyG,KeyB}|{g,KeyR,KeyB}|{b,KeyR,KeyG}|
%%                  {h,KeyS,KeyV}|{s,KeyH,KeyV}|{v,KeyH,KeyS}
%% The 'range' flag is mandatory.
%% Do not use other ranges than 0.0 through 1.0 for R,G,B,H,S color sliders,
%% nor other than 0.0 through 360.0 for a V color slider.
%% The Key* keys are used to read in the other color properties for the
%% slider to draw a sensible background color.
%% The same Flags are passed on to the {text,_,_} or {color,_,_} fields.
%% Returns a value only if the 'value' flag is used.
%%
%% {text,Def[,Flags]}				-- Numerical or text field
%%     Def = integer()|float()|String  -- Start value
%%     Flags = {range,{Min,Max}}|{width,CharW}|{password,Bool}|
%%             {charset,Charset}|RegularFlag
%%     CharW = integer() >= 1
%%         %% Min and Max should be of same type as Def and is not
%%         %% allowed with a String field. 
%%         %% Min can also be '-infinity' and Max 'infinity'.
%%         %% CharW is in characters. Default width is 30 for
%%         %% String fields, 8 for integer() and 12 for float().
%%         %% Charset is latin1 or unicode; default is latin1.
%%
%% {String,Bool[,RegularFlags]}			-- Checkbox
%%
%% {help,Title,HelpLines}                       -- Help button
%%     Title = String
%%     HelpLines = [HelpLine] | fun() -> [HelpLine]
%%     HelpLine = String
%%
%%
%% What happens?
%% -------------
%%
%% mktree/2:
%% The querys (Qs) are first converted into a tree structure, stored 
%% in #s.fi. In this pass, the values in the store (#fi.store) are initialized.
%%
%% layout/2 and layout_propagate/1:
%% Then the dialog is layouted. This pass sets the #fi.x and #fi.y 
%% positions as well as the minimum acceptable #fi.w and #fi.h for
%% each field. Container fields asks their contained fields to get
%% a suitable size for the container. Leaf fields have their minimum
%% size hardcoded. The layout_propagate subpass goes over the fields
%% once more and propagates the containers resulting size down to
%% their contained fields.
%%
%% focusable/1:
%% Traverses the tree and builds a tuple of field indexes of all 
%% fields that are enabled for keyboard or mouse events.
%%
%% redraw/1:
%% Draws all fields in the tree. Any field can during this pass tell
%% that it is disabled, forcing the focusable tuple to be updated.
%%

dialog(false, _Title, Qs, Fun) ->
    S = setup_dialog(Qs, Fun),
    return_result(S#s{owner=wings_wm:this()}),
    keep;
dialog(true, Title, Qs, Fun) -> dialog(Title, Qs, Fun).

dialog(Title, Qs, Fun) ->
    do_dialog(Title, Qs, [make_ref()], Fun).

do_dialog(Title, Qs, Level, Fun) ->
    GrabWin = wings_wm:release_focus(),
    Owner = wings_wm:this(),
    S0 = #s{w=W,h=H,fi=Fi,store=Store} = setup_dialog(Qs, Fun),
    S = S0#s{level=Level,grab_win=GrabWin,owner=Owner},
    Name = {dialog,hd(Level)},
    setup_blanket(Name, Fi, Store),
    Op = get_event(S),				%No push - replace crash handler.
    {_,Xm,Ym} = wings_io:get_mouse_state(),
    wings_wm:toplevel(Name, Title, {Xm,Ym-?LINE_HEIGHT}, {W,H}, 
		      [{anchor,n}], Op),
    wings_wm:set_prop(Name, drag_filter, fun(_) -> yes end),
    ?DEBUG_DISPLAY(other, {W,H}),
    keep.

setup_dialog(Qs, Fun) ->
    {Fi0,Sto,N} = mktree(Qs, gb_trees:empty()),
    Fi = #fi{w=W0,h=H0} = layout(Fi0, Sto),
    Focusable = focusable(Fi),
    W = W0 + 2*?HMARGIN,
    H = H0 + 2*?VMARGIN,
    next_focus(1, #s{ox=?HMARGIN,oy=?VMARGIN,w=W,h=H,
		     call=Fun,focus=N,focusable=Focusable,
		     fi=Fi,n=N,store=Sto}).

setup_blanket(Dialog, Fi, Sto) ->
    EyePicker = case find_eyepicker(Fi, Sto) of
		    [#fi{}|_] -> true;
		    [] -> false
		end,
    %% The menu blanket window lies below the dialog, covering the entire
    %% screen and ignoring most events. Keyboard events will be forwarded
    %% to the active dialog window.
    Op = {push,fun(Ev) -> blanket(Ev, Dialog, EyePicker) end},
    {TopW,TopH} = wings_wm:top_size(),
    wings_wm:new({blanket,Dialog}, {0,0,highest}, {TopW,TopH}, Op).

delete_blanket(#s{level=[Level|_]}) ->
    wings_wm:delete({blanket,{dialog,Level}});
delete_blanket(#s{level=undefined}) -> ok.

blanket(#keyboard{}=Ev, Dialog, _) ->
    wings_wm:send(Dialog, Ev);
blanket(#mousemotion{}, _, true) ->
    wings_io:eyedropper(),
    keep;
blanket(#mousebutton{button=1,state=?SDL_RELEASED,x=X0,y=Y0}, Dialog, true) ->
    {X,Y0} = wings_wm:local2global(X0, Y0),
    {_,H} = wings_wm:top_size(),
    Y = H - Y0,
    Mem = wings_io:get_buffer(3, ?GL_UNSIGNED_BYTE),
    gl:readBuffer(?GL_FRONT),
    gl:readPixels(X, Y, 1, 1, ?GL_RGB, ?GL_UNSIGNED_BYTE, Mem),
    gl:readBuffer(?GL_BACK),
    [R,G,B] = wings_io:read_buffer(Mem, ?GL_UNSIGNED_BYTE, 3),
    Col = {R/255,G/255,B/255},
    wings_wm:send(Dialog, {picked_color,Col}),
    keep;
blanket(_, _, _) -> keep.

get_event(S) ->
    wings_wm:dirty(),
    {replace,fun(Ev) -> event1(Ev, S) end}.

event1({crash,Reason}, S) ->
    %% dmptree(S#s.fi),
    wings_u:win_crash(Reason),
    delete(S);
event1(redraw, S) ->
    ?DEBUG_DISPLAY(event, redraw),
    redraw(S);
event1(got_focus, _) ->
    wings_wm:message(" "),
    keep;
event1(lost_focus, _) ->
    wings_wm:message("  "),
    keep;
event1(Ev=#mousemotion{}, S) ->
    mouse_event(Ev, S);
event1(Ev, S) -> 
    wings_wm:message(""),
    event(Ev, S).

event({current_state,_}, _) ->
    keep;
event(#keyboard{sym=Sym,mod=Mod,unicode=Unicode}, S) ->
    event_key({key,Sym,Mod,Unicode}, S);
event(#mousebutton{}=Ev, S) ->
    maybe_doubleclick(Ev),
    mouse_event(Ev, S);
event({doubleclick,_,_}=Ev, S) ->
    mouse_event(Ev, S);
event({drop,{X,Y},DropData}, S) ->
    drop_event(X, Y, DropData, S);
event({action,Action}, S) ->
    field_event(Action, S);
event(Ev={picked_color,_}, #s{fi=Fi,store=Sto}=S) ->
    field_event(Ev, S, find_eyepicker(Fi, Sto));
event(Ev, S) -> field_event(Ev, S).

%% Dialog keys, not passed down as field events
event_key({key,?SDLK_ESCAPE,_,_}, S) ->
    escape_pressed(S);
event_key({key,_,_,$\e}, S) ->
    escape_pressed(S);
event_key({key,?SDLK_TAB,Mod,_}, S) when ?IS_SHIFTED(Mod) ->
    get_event(next_focus(-1, S));
event_key({key,?SDLK_TAB,_,_}, S) ->
    get_event(next_focus(+1, S));
event_key({key,_,Mod,$\t}, S) when ?IS_SHIFTED(Mod) ->
    get_event(next_focus(+1, S));
event_key({key,_,_,$\t}, S) ->
    get_event(next_focus(+1, S));
event_key({key,?SDLK_KP_ENTER,Mod,_}, S) ->
    enter_pressed({key,$\r,Mod,$\r}, S);
event_key({key,_,_,$\r}=Ev, S) ->
    enter_pressed(Ev, S);
%% Common key translations; emacs editing style
event_key({key,Sym=?SDLK_BACKSPACE,Mod,_}, S) ->
    field_event({key,Sym,Mod,$\b}, S);
event_key({key,Sym=?SDLK_DELETE,Mod,_}, S) ->
    field_event({key,Sym,Mod,$\^D}, S);
event_key({key,Sym=?SDLK_HOME,Mod,_}, S) ->
    field_event({key,Sym,Mod,$\^A}, S);
event_key({key,Sym=?SDLK_END,Mod,_}, S) ->
    field_event({key,Sym,Mod,$\^E}, S);
event_key({key,Sym=?SDLK_LEFT,Mod,_}, S) ->
    field_event({key,Sym,Mod,$\^B}, S);
event_key({key,Sym=?SDLK_RIGHT,Mod,_}, S) ->
    field_event({key,Sym,Mod,$\^F}, S);
event_key({key,Sym=?SDLK_UP,Mod,_}, S) ->
    field_event({key,Sym,Mod,$\^P}, S);
event_key({key,Sym=?SDLK_DOWN,Mod,_}, S) ->
    field_event({key,Sym,Mod,$\^N}, S);
%% Common key translations; keypad
event_key({key,Sym=?SDLK_KP_PLUS,Mod,_}, S) ->
    field_event({key,Sym,Mod,$+}, S);
event_key({key,Sym=?SDLK_KP_MINUS,Mod,_}, S) ->
    field_event({key,Sym,Mod,$-}, S);
event_key({key,Sym=?SDLK_KP_MULTIPLY,Mod,_}, S) ->
    field_event({key,Sym,Mod,$*}, S);
event_key({key,Sym=?SDLK_KP_DIVIDE,Mod,_}, S) ->
    field_event({key,Sym,Mod,$/}, S);
event_key({key,Sym=?SDLK_KP_PERIOD,Mod,_}, S) ->
    field_event({key,Sym,Mod,$.}, S);
event_key({key,Sym=?SDLK_KP_EQUALS,Mod,_}, S) ->
    field_event({key,Sym,Mod,$=}, S);
event_key({key,Sym,Mod,_}, S) when ?SDLK_KP0 =< Sym, Sym =< ?SDLK_KP9 ->
    field_event({key,Sym,Mod,$0+Sym-?SDLK_KP0}, S);
%%
event_key(Ev, S) ->
    field_event(Ev, S).

enter_pressed(Ev, #s{focus=I,fi=TopFi,store=Store}=S0) ->
    %% Try the following in order:
    %% 1. If the current field is a button, send the event to it.
    %% 2. If there is an OK button, send the event to it;
    %%    otherwise finish the dialog returning the result.
    case field_type(I, Store) of
	but -> field_event(Ev, S0);
	_ ->
	    case find_ok(TopFi, Store) of
		[] -> return_result(S0);
		Path -> 
		    S = set_focus(Path, S0, false),
		    case field_event(Ev, S, Path) of
			keep -> get_event(S);
			Other -> Other
		    end
	    end
    end.

escape_pressed(S0=#s{fi=TopFi,store=Sto}) -> 
    case find_cancel(TopFi, Sto) of
	[] -> keep;
	Path -> 
	    S = set_focus(Path, S0, false),
	    case field_event({key,$\s,0,$\s}, S, Path) of
		keep -> get_event(S);
		Other -> Other
	    end
    end.

delete(#s{level=[_],grab_win=GrabWin}=S) ->
    delete_blanket(S),
    wings_wm:grab_focus(GrabWin),
    wings_wm:allow_drag(false),
    delete;
delete(S) ->
    delete_blanket(S),
    delete.

maybe_doubleclick(#mousebutton{button=1,state=?SDL_RELEASED}=Ev) ->
    put(wings_ask_doubleclick, 
	maybe_doubleclick_1(Ev, get(wings_ask_doubleclick)));
maybe_doubleclick(_) -> ok.

maybe_doubleclick_1(#mousebutton{x=X,y=Y}, undefined) ->
    {X,Y,now()};
maybe_doubleclick_1(#mousebutton{x=X,y=Y}, {X0,Y0,Now0}) ->
    Dx = X-X0, Dy = Y-Y0,
    Now = now(),
    case math:sqrt(Dx*Dx+Dy*Dy) of
	Dist when Dist < 5 ->
	    case timediff(Now, Now0) of
		TimeDiff when TimeDiff < 0.25 ->
		    wings_wm:later({doubleclick,X,Y});
		_TimeDiff -> ok
	    end;
	_Dist -> ok
    end,
    {X,Y,Now}.

timediff({A,B,C}, {D,E,F}) ->
    1000000*(A-D) + B-E + (C-F)/1000000.

mouse_event(Ev=#mousemotion{x=X0,y=Y0}, #s{ox=Ox,oy=Oy}=S) ->
    X = X0-Ox,
    Y = Y0-Oy,
    field_event(Ev#mousemotion{x=X,y=Y}, S);
mouse_event(Ev=#mousebutton{x=X0,y=Y0,state=State}, 
	    S0=#s{mouse_focus=MouseFocus,ox=Ox,oy=Oy,fi=TopFi}) ->
    X = X0-Ox,
    Y = Y0-Oy,
    case State of
	?SDL_RELEASED ->
	    case MouseFocus of
		true -> field_event(Ev#mousebutton{x=X,y=Y}, S0);
		false -> keep
	    end;
	?SDL_PRESSED ->
	    case mouse_to_field(X, Y, TopFi) of
		Path=[#fi{state=enabled}|_] ->
		    S = set_focus(Path, S0, true),
		    case field_event(Ev#mousebutton{x=X,y=Y}, S, Path) of
			keep -> get_event(S);
			Other -> Other
		    end;
		_ -> get_event(S0#s{mouse_focus=false})
	    end
    end;
mouse_event({doubleclick,X0,Y0}, #s{mouse_focus=MouseFocus,ox=Ox,oy=Oy}=S) ->
    X = X0-Ox,
    Y = Y0-Oy,
    case MouseFocus of
	true -> field_event({doubleclick,X,Y}, S);
	false -> keep
    end.

drop_event(X, Y, DropData, #s{ox=Ox,oy=Oy,fi=TopFi}=S0) ->
    case mouse_to_field(X-Ox, Y-Oy, TopFi) of
	Path=[#fi{state=enabled}|_] ->
	    S = set_focus(Path, S0, true),
	    case field_event({drop,DropData}, S, Path) of
		keep -> get_event(S);
		Other -> Other
	    end;
	_ -> get_event(S0#s{mouse_focus=false})
    end.


next_focus(Dir, S=#s{focus=Index,focusable=Focusable,fi=TopFi}) ->
    J = case binsearch(fun (I) when I < Index -> -1;
			   (I) when Index < I -> +1;
			   (_) -> 0 end,
		       Focusable) of
	    {I,_} when 0 < Dir -> I;
	    {_,I} -> I;
	    I -> I
	end,
    N = tuple_size(Focusable),
    case (J+Dir) rem N of
	K when K =< 0 -> 
	    Path = get_fi(element(N+K, Focusable), TopFi),
	    set_focus(Path, S, false);
	K -> 
	    Path = get_fi(element(K, Focusable), TopFi),
	    set_focus(Path, S, false)
    end.

set_focus([#fi{index=Index}|_], #s{focus=Index}=S, MouseFocus) ->
    %% Focus on the same field. Do NOT call the handlers -
    %% the text field depends on it.
    S#s{mouse_focus=MouseFocus};
set_focus([#fi{index=NewIndex,handler=NewHandler}|_]=NewPath,
	  #s{focus=OldIndex,focusable=_Focusable,
	     fi=TopFi,store=Store0}=S, MouseFocus) ->
    ?DEBUG_DISPLAY(other, {set_focus,[OldIndex,NewIndex,_Focusable]}),
    [#fi{handler=OldHandler}|_] = OldPath = get_fi(OldIndex, TopFi),
    Store2 = case OldHandler({focus,false}, OldPath, Store0) of
		 {store,Store1} -> Store1;
		 _ -> Store0
	     end,
    Store = case NewHandler({focus,true}, NewPath, Store2) of
		{store,Store3} -> Store3;
		_ -> Store2
	    end,
    S#s{focus=NewIndex,mouse_focus=MouseFocus,store=Store}.


field_event(Ev=#mousemotion{x=X,y=Y,state=Bst}, S=#s{fi=Fi})
  when (Bst band ?BUTTON_MASK) =:= 0 ->
    case mouse_to_field(X, Y, Fi) of
	Path=[#fi{index=_I,state=enabled,flags=Flags,extra=Extra}|_] ->
	    ?DEBUG_DISPLAY(event, {field_event,[_I,Ev]}),
	    wings_wm:allow_drag(member(drag, Flags)),
	    case Extra of
		#container{} -> field_event(Ev, S, Path);
		_ -> 
		    case proplists:get_value(info, Flags) of
			undefined -> wings_wm:message("");
			Info -> wings_wm:message(Info)
		    end
	    end;
	_ ->
	    wings_wm:allow_drag(false),
	    wings_wm:message("")
    end, keep;
field_event({key,_,_,_}=Ev, S=#s{focus=I,fi=Fi}) ->
    %% Key events should always be sent, even if there
    %% is no mouse focus. Otherwise you can't TAB to
    %% text fields.
    field_event(Ev, S, get_fi(I, Fi));
field_event({popup_result,_}=Ev, S=#s{focus=I,fi=Fi}) ->
    %% As key events above.
    field_event(Ev, S, get_fi(I, Fi));
field_event(Ev, S=#s{focus=I,mouse_focus=true,fi=Fi}) ->
    field_event(Ev, S, get_fi(I, Fi));
field_event(_Ev, _S) -> keep.

field_event(Ev, S=#s{focus=_I,fi=TopFi=#fi{w=W0,h=H0},store=Store0}, 
	    Path=[#fi{handler=Handler}|_]) ->
    ?DEBUG_DISPLAY(event, {field_handler,[_I,Ev]}),
    Result = Handler(Ev, Path, Store0),
    ?DEBUG_DISPLAY(event, {field_handler,
			   if is_tuple(Result) -> {element(1, Result)};
			      true -> Result end}),
    case Result of
	ok -> return_result(S);
	done -> return_result(S);
	{done,Store} -> return_result(S#s{store=Store});
	cancel -> delete(S);
	keep -> keep;
	{recursive,Return} -> Return;
	{drag,{_,_}=WH,DropData} -> wings_wm:drag(Ev, WH, DropData);
	{store,Store} -> get_event(S#s{store=Store});
	{layout,Store} ->
	    Fi = #fi{w=W,h=H} = layout(TopFi, Store),
	    ?DMPTREE(tree, Fi),
	    Focusable = focusable(Fi),
	    ?DEBUG_DISPLAY(other, {new_focusable,Focusable}),
	    case {W,H} of
		{W0,H0} -> ok;
		_ -> 
		    Size = {W+2*?HMARGIN,H+2*?VMARGIN},
		    maybe_resize_move(Size)
	    end,
	    ?DEBUG_DISPLAY(other, {W,H}),
	    get_event(next_focus(0, S#s{w=W,h=H,focusable=Focusable,
					fi=Fi,store=Store}));
	{Action,Store} when is_function(Action, 1) ->
	    Res = collect_result(TopFi, Store),
	    Action(Res),
	    delete(S);
	Action when is_function(Action, 1) ->
	    Res = collect_result(TopFi, Store0),
	    Action(Res),
	    delete(S)
    end.

maybe_resize_move({W,H0}=Size0) ->
    This = wings_wm:this(),
    Size = case wings_wm:win_size(This) of
	       {OldW,OldH}=OldSize when W =< OldW, H0 =< OldH -> OldSize;
	       _ -> Size0
	   end,
    {{X1,Y1},{_,H1}} = wings_wm:win_rect({controller,This}),
    H = H0+H1,
    {{X3,Y3},{W3,H3}} = wings_wm:win_rect(desktop),
    X = if  W > W3 -> X3;
	    X1+W > W3 -> W3-W;
	    true -> X1 end,
    Y = if  H > H3 -> Y3;
	    Y1+H > H3 -> H3-H;
	    true -> Y1 end,
    wings_wm:move(This, {X,Y+H1}, Size).


return_result(#s{call=EndFun,owner=Owner,fi=Fi,store=Sto,
		 level=Level,grab_win=GrabWin}=S0) ->
    Res = collect_result(Fi, Sto),
    ?DEBUG_DISPLAY(other, {return_result,Res}),
    case EndFun(Res) of
	ignore ->
	    delete(S0);
	#st{}=St ->
	    ?DEBUG_DISPLAY(other, {return_result,[Owner,new_state]}),
	    wings_wm:send(Owner, {new_state,St}),
	    delete(S0);
	{dialog,Qs,Fun} ->
	    S = #s{w=W,h=H} = setup_dialog(Qs, Fun),
	    maybe_resize_move({W,H}),
	    get_event(S#s{level=Level,grab_win=GrabWin,owner=Owner});
	Action when is_tuple(Action); is_atom(Action) ->
	    ?DEBUG_DISPLAY(other, {return_result,[Owner,{action,Action}]}),
	    wings_wm:send(Owner, {action,Action}),
	    delete(S0)
    end.

redraw(S=#s{ox=Ox,oy=Oy,focus=Index,fi=Fi0,store=Sto}) ->
    ?DEBUG_DISPLAY(event, redraw),
    wings_io:ortho_setup(),
    {W,H} = wings_wm:win_size(),
    blend(fun(Col) ->
		  wings_io:border(0, 0, W-1, H-1, Col)
	  end),
    gl:translated(Ox, Oy, 0),
    case draw_fields(Fi0, Index, Sto) of
	keep -> 
	    ?DEBUG_DISPLAY(other, {draw_fields,keep}),
	    keep;
	Fi ->
	    Focusable = focusable(Fi),
	    ?DEBUG_DISPLAY(other, {new_focusable,Focusable}),
	    get_event(next_focus(0, S#s{focusable=Focusable,fi=Fi}))
    end.

%% Binary search a tuple. Cmp should return integer() > 0 | 0 | 0 < integer().
%% Return {I,I+1} | {I-1,I} if not found, or I if found,
%% where integer(I), 1 =< I, I =< Size,
%% where Size = tuple_size(Tuple).

binsearch(Cmp, Tuple) when is_function(Cmp, 1), tuple_size(Tuple) > 0 ->
    binsearch(Cmp, Tuple, 1, tuple_size(Tuple)).

binsearch(Cmp, Tuple, L, U) when L =< U ->
    I = (L + U) div 2,
    C = Cmp(element(I, Tuple)),
    case I of
	%% L == U
	U when C < 0 -> {U,U+1};
	U when 0 < C -> {U-1,U};
	%% L == U-1
	L when C < 0 -> binsearch(Cmp, Tuple, U, U);
	L when 0 < C -> {L-1,L};
	%% L < I < U
	_ when 0 < C -> binsearch(Cmp, Tuple, L, I-1);
	_ when C < 0 -> binsearch(Cmp, Tuple, I+1, U);
	_ -> I
    end.
    
%%%
%%% Dialog tree functions
%%%

find_ok(Fi, Sto) ->
    find_field(fun([#fi{index=Index,flags=Flags}|_]) ->
		       (field_type(Index, Sto) =:= but)
			   andalso proplists:get_bool(ok, Flags)
	       end, Fi).

find_cancel(Fi, Sto) ->
    find_field(fun([#fi{index=Index,flags=Flags}|_]) ->
		       (field_type(Index, Sto) =:= but)
			   andalso proplists:get_bool(cancel, Flags)
	       end, Fi).

find_eyepicker(Fi, Sto) ->
    find_field(fun([#fi{index=Index}|_]) ->
		       field_type(Index, Sto) =:= eyepicker
	       end, Fi).

field_type(I, Store) ->
    case gb_trees:lookup(-I, Store) of
	{value,F} -> element(1, F);
	none -> undefined
    end.

%% Search tree to find field matching a predicate using reverse linear scan.

find_field(Fun, Fi) -> find_field_1(Fun, Fi, []).

find_field_1(Fun, Fi=#fi{extra=#container{fields=Fields}}, Path) ->
    find_field_2(Fun, Fields, [Fi|Path], tuple_size(Fields));
find_field_1(Fun, Fi, Path0) ->
    Path = [Fi|Path0],
    case Fun(Path) of
	false -> [];
	true -> Path
    end.

find_field_2(Fun, Fields, Path, I) when I >= 1 ->
    case find_field_1(Fun, element(I, Fields), Path) of
	[] -> find_field_2(Fun, Fields, Path, I-1);
	P -> P
    end;
find_field_2(_Fun, _Fields, _Path, _I) -> [].

%% Collect results from all fields.
%% Return list of result values.

collect_result(Fi, Sto) ->  reverse(collect_result_1(Fi, Sto, [], [])).

collect_result_1(Fi=#fi{state=inert}, Sto, Path, R) ->
    collect_result_2(Fi, Sto, Path, R);
collect_result_1(Fi=#fi{handler=Handler,key=Key}, Sto, Path, R0) ->
    R = case Handler(value, [Fi|Path], Sto) of
        keep -> R0;
	    none -> R0;
	    {value,Res} when is_integer(Key) -> [Res|R0];
	    {value,Res} -> [{Key,Res}|R0]
	end,
    collect_result_2(Fi, Sto, Path, R).

collect_result_2(Fi=#fi{extra=#container{fields=Fields}}, Sto, Path, R) ->
    collect_result_3(Fields, Sto, [Fi|Path], R, 1);
collect_result_2(_Fi, _Sto, _Path, R) -> R.

collect_result_3(Fields, Sto, Path, R, I) when I =< tuple_size(Fields) ->
    collect_result_3(Fields, Sto, Path,
		     collect_result_1(element(I, Fields), Sto, Path, R),
		     I+1);
collect_result_3(_Fields, _Sto, _Path, R, _I) -> R.

%% Draw fields.
%% Return new fields tree.

draw_fields(Fi, Focus, Sto) -> draw_fields_1(Fi, Focus, enabled, Sto, []).

%% Container; minimized|maximized|undefined, enabled|disabled|inert
draw_fields_1(#fi{handler=Handler,key=Key,index=Index,hook=Hook,
		  state=State,minimized=Minimized,
		  extra=Container=#container{fields=Fields0,
					     selected=Selected}}=Fi,
	      Focus, DisEnabled0, Sto, Path0) ->
    DisEnabled = 
	case DisEnabled0 of
	    disabled -> disabled;
	    enabled ->
		case hook(Hook, is_disabled, [var(Key, Index), Index, Sto]) of
		    keep -> enabled;
		    DisEn -> DisEn
		end
	end,
    Path = [Fi|Path0],
    Handler({redraw,Index =:= Focus,State}, Path, Sto),
    case {Minimized,Selected} of
	{true,_} -> keep;
	{_,undefined} ->
	    case draw_fields_2(Fields0, Focus, DisEnabled, 
			       Sto, Path, 1, [], false) of
		keep -> keep;
		Fields -> Fi#fi{extra=Container#container{fields=Fields}}
	    end;
	{_,_} ->
	    case draw_fields_1(element(Selected, Fields0), 
			       Focus, DisEnabled, Sto, Path) of
		keep -> keep;
		SelFi -> 
		    Fields = setelement(Selected, Fields0, SelFi),
		    Fi#fi{extra=Container#container{fields=Fields}}
	    end
    end;
%% Leaf; minimized
draw_fields_1(#fi{state=_State,minimized=true}=_Fi, 
	      _Focus, _DisEnabled, _Sto, _Path) -> 
    keep;
%% Leaf; maximized|undefined, enabled|disabled|inert
draw_fields_1(#fi{handler=Handler,key=Key,index=Index,hook=Hook,
		  state=State0}=Fi0, 
	      Focus, DisEnabled, Sto, Path) ->
    State = 
	case {State0,DisEnabled} of
	    {inert,_} -> inert;
	    {_,disabled} -> disabled;
	    {_,enabled} -> 
		case hook(Hook, is_disabled, [var(Key, Index), Index, Sto]) of
		    keep -> DisEnabled;
		    St -> St
		end
	end,
    Fi = case State of
	     State0 -> Fi0;
	     _ -> Fi0#fi{state=State}
	 end,
    Handler({redraw,Index =:= Focus, State}, [Fi|Path], Sto),
    case State of
	State0 -> keep;
	_ -> Fi
    end.

draw_fields_2(Fields, Focus, DisEnabled, Sto, TopFi, I, R, Changed)
  when I =< tuple_size(Fields) ->
    Fi0 = element(I, Fields),
    case draw_fields_1(Fi0, Focus, DisEnabled, Sto, TopFi) of
	keep -> draw_fields_2(Fields, Focus, DisEnabled, 
			      Sto, TopFi, I+1, [Fi0|R], Changed);
	Fi -> draw_fields_2(Fields, Focus, DisEnabled,
			    Sto, TopFi, I+1, [Fi|R], true)
    end;
draw_fields_2(_Fields, _Focus, _DisEnabled, _Sto, _TopFi, _I, _R, false) ->
    keep;
draw_fields_2(_Fields, _Focus, _DisEnabled, _Sto, _TopFi, _I, R, true) ->
    list_to_tuple(reverse(R)).

%% Get field index from mouse position.
%% Return path to root.

mouse_to_field(X, Y, Fi) -> mouse_to_field_1(X, Y, Fi, [Fi]).

mouse_to_field_1(X, Y, 
		 Fi0=#fi{index=_Index,minimized=Minimized,
			 extra=#container{x=X0,y=Y0,w=W,h=H,
					  type=Type,fields=Fields,
					  selected=Selected}},
		 Path0)
  when Minimized =/= true, X0 =< X, X < X0+W, Y0 =< Y, Y < Y0+H ->
    ?DEBUG_DISPLAY(event, [_Index,X,Y,X0,Y0,W,H]),
    case Selected of
	undefined -> mouse_to_field_2(X, Y, Fields, Path0, Type);
	_ ->
	    Fi = element(Selected, Fields),
	    mouse_to_field_1(X, Y, Fi, [Fi,Fi0|Path0])
    end;
mouse_to_field_1(X, Y, #fi{index=_Index,x=X0,y=Y0,w=W,h=H}, Path)
  when X0 =< X, X < X0+W, Y0 =< Y, Y < Y0+H ->
    ?DEBUG_DISPLAY(event, [_Index,X,Y,X0,Y0,W,H]),
    Path;
mouse_to_field_1(_X, _Y, #fi{}, _Path) ->
    [].

mouse_to_field_2(Xm, Ym, Fields, Path, vframe) ->
    Cmp = fun (#fi{y=Y,h=H}) when Y+H =< Ym -> -1;
	      (#fi{y=Y}) when Ym < Y -> 1;
	      (#fi{}) -> 0 end,
    Index = case binsearch(Cmp, Fields) of
%		{0,1} -> 1;
		{I,_} -> I;
		I -> I
	    end,
    Fi = element(Index, Fields),
    mouse_to_field_1(Xm, Ym, Fi, [Fi|Path]);
mouse_to_field_2(Xm, Ym, Fields, Path, hframe) ->
    Cmp = fun (#fi{x=X,w=W}) when X+W =< Xm -> -1;
	      (#fi{x=X}) when Xm < X -> 1;
	      (#fi{}) -> 0 end,
    Index = case binsearch(Cmp, Fields) of
%		{0,1} -> 1;
		{I,_} -> I;
		I -> I
	    end,
    Fi = element(Index, Fields),
    mouse_to_field_1(Xm, Ym, Fi, [Fi|Path]).
    
%% Get field from field index
%% Return path to root.

get_fi(Index, Fi) -> get_fi_1(Index, Fi, [Fi]).

get_fi_1(Index, #fi{index=Index}, Path) ->
    Path;
get_fi_1(Index, #fi{extra=#container{fields=Fields}}, Path) ->
    Cmp = fun (#fi{index=I}) when I < Index -> -1;
	      (#fi{index=I}) when Index < I -> 1;
	      (_) -> 0 end,
    case binsearch(Cmp, Fields) of
	{0,1} -> [];
	{I,_} -> 
	    Fi = element(I, Fields),
	    get_fi_1(Index, Fi, [Fi|Path]);
	I -> [element(I, Fields)|Path]
    end.% ;
% get_fi_1(_Index, #fi{extra=#leaf{}}) ->
%     [].



%%
%% Conversion of dialog query into internal tree format
%%

mktree(Qs0, Sto0) when is_list(Qs0) ->
    Qs = {hframe,[{vframe,Qs0},
		  {vframe,[{button,ok,[ok]},
			   {button,cancel,[cancel]}]}]},
    {Fis,Sto,I} = mktree(Qs, Sto0, 1),
    {Fis,Sto,I-1};
mktree(Qs, Sto0) ->
    {Fis,Sto,I} = mktree(Qs, Sto0, 1),
    {Fis,Sto,I-1}.

mktree({label_column,Qs0}, Sto, I) ->
    {Labels,Fields} = dialog_unzip(Qs0),
    Qs = {hframe,
	  [{vframe,Labels},
	   {vframe,Fields}]},
    mktree(Qs, Sto, I);
%%
mktree({vradio,Qs,Def}, Sto, I) ->
    mktree(radio(vframe, Qs, Def, []), Sto, I);
mktree({vradio,Qs,Def,Flags}, Sto, I) ->
    mktree(radio(vframe, Qs, Def, Flags), Sto, I);
mktree({hradio,Qs,Def}, Sto, I) ->
    mktree(radio(hframe, Qs, Def, []), Sto, I);
mktree({hradio,Qs,Def,Flags}, Sto, I) ->
    mktree(radio(hframe, Qs, Def, Flags), Sto, I);
%%
mktree({vframe,Qs}, Sto, I) ->
    mktree_container(Qs, Sto, I, [layout], vframe);
mktree({vframe,Qs,Flags}, Sto, I) ->
    mktree_container(Qs, Sto, I, [layout|Flags], vframe);
mktree({hframe,Qs}, Sto, I) ->
    mktree_container(Qs, Sto, I, [layout], hframe);
mktree({hframe,Qs,Flags}, Sto, I) ->
    mktree_container(Qs, Sto, I, [layout|Flags], hframe);
mktree({oframe,Qs,Def}, Sto, I) when is_integer(Def) ->
    mktree_oframe(Qs, Def, Sto, I, [layout]);
mktree({oframe,Qs,Def,Flags}, Sto, I) when is_integer(Def), is_list(Flags) ->
    mktree_oframe(Qs, Def, Sto, I, [layout|Flags]);
%%
mktree(panel, Sto, I) ->
    mktree_panel(Sto, I, [{stretch,1}]);
mktree({panel,Flags}, Sto, I) ->
    mktree_panel(Sto, I, Flags);
%%
mktree({value,Value}, Sto, I) ->
    mktree_value(Value, Sto, I, []);
mktree({value,Value,Flags}, Sto, I) ->
    mktree_value(Value, Sto, I, Flags);
%%
mktree({eyepicker,Hook}, Sto, I) ->
    mktree_eyepicker(Sto, I, [{hook,Hook}]);
%%
mktree({label,Label}, Sto, I) ->
    mktree_label(Label, Sto, I, []);
mktree({label,Label,Flags}, Sto, I) ->
    mktree_label(Label, Sto, I, Flags);
%%
mktree({color,Def}, Sto, I) ->
    mktree_color(Def, Sto, I, [drag]);
mktree({color,Def,Flags}, Sto, I) ->
    mktree_color(Def, Sto, I, [drag|Flags]);
%%
mktree({alt,Def,Prompt,Val}, Sto, I) ->
    mktree_radiobutton(Def, Prompt, Val, Sto, I, []);
mktree({alt,Def,Prompt,Val,Flags}, Sto, I) ->
    mktree_radiobutton(Def, Prompt, Val, Sto, I, Flags);
mktree({key_alt,{Key,Def},Prompt,Val}, Sto, I) ->
    mktree_radiobutton(Def, Prompt, Val, Sto, I, [{key,Key}]);
mktree({key_alt,{Key,Def},Prompt,Val,Flags}, Sto, I) ->
    mktree_radiobutton(Def, Prompt, Val, Sto, I, [{key,Key}|Flags]);
%%
mktree({menu,Menu,Def}, Sto, I) ->
    mktree_menu(Menu, Def, Sto, I, []);
mktree({menu,Menu,Def,Flags}, Sto, I) when is_list(Flags) ->
    mktree_menu(Menu, Def, Sto, I, Flags);
%%
mktree({button,{text,_Def,Flags}=TextField}, Sto, I) ->
    mktree({hframe,[TextField,
		    {button,?__(1,"Browse"),keep,
		     [{hook,browse_hook_fun(-1, Flags)},
		      {drop_flags,[{index,-1},
				   {hook,drop_hook_fun(Flags)}
				   |Flags]}
		      |proplists:delete(key, Flags)]}]}, 
	   Sto, I);
mktree({button,Action}, Sto, I) when is_atom(Action) ->
    mktree_button(Action, Sto, I, []);
mktree({button,Action,Flags}, Sto, I) when is_atom(Action), is_list(Flags) ->
    mktree_button(Action, Sto, I, Flags);
mktree({button,Label,Action}, Sto, I) ->
    mktree_button(Label, Action, Sto, I, []);
mktree({button,Label,Action,Flags}, Sto, I) ->
    mktree_button(Label, Action, Sto, I, Flags);
%%
mktree({help,Title,HelpLines}, Sto, I) ->
    mktree({button,"?",keep,[help_hook(Title,HelpLines),{width,1}]}, Sto, I);
%%
mktree({custom,W,H,Custom}, Sto, I) ->
    mktree_custom(W, H, Custom, Sto, I, []);
mktree({custom,W,H,Custom,Flags}, Sto, I) ->
    mktree_custom(W, H, Custom, Sto, I, Flags);
%%
mktree({slider,{text,_,Flags}=Field}, Sto, I) ->
    SliderFlags = case proplists:get_value(key, Flags, 0) of
		      K when is_integer(K) -> [{key,K-1}|Flags];
		      _ -> Flags
		  end,
    mktree({hframe,[Field,{slider,SliderFlags}]}, Sto, I);
mktree({slider,{color,_,Flags}=Field}, Sto, I) ->
    SliderFlags0 = case proplists:get_value(key, Flags, 0) of
		       K when is_integer(K) -> [{key,K+1}|Flags];
		       _ -> Flags
		   end,
    SliderFlags = [{range,{0.001,1.0}},{color,true}|SliderFlags0],
    mktree({hframe,[{slider,SliderFlags},Field]}, Sto, I);
mktree({slider,Flags}, Sto, I) when is_list(Flags) ->
    mktree_slider(Sto, I, Flags);
%%
mktree(separator, Sto, I) ->
    mktree_separator(Sto, I, []);
%%
mktree({table,Elements}, Sto, I) ->
    mktree_table(Elements, Sto, I, []);
mktree({table,Elements,Flags}, Sto, I) ->
    mktree_table(Elements, Sto, I, Flags);
%%
mktree({text,Def}, Sto, I) ->
    mktree_text(Def, Sto, I, []);
mktree({text,Def,Flags}, Sto, I) ->
    mktree_text(Def, Sto, I, Flags);
%%
mktree({Prompt,Def}, Sto, I) when Def==false; Def == true ->
    mktree_checkbox(Prompt, Def, Sto, I, []);
mktree({Prompt,Def,Flags}, Sto, I) when Def==false; Def == true ->
    mktree_checkbox(Prompt, Def, Sto, I, Flags).

radio(FrameType, Qs0, Def, Flags) ->
    Qs = 
	case proplists:get_value(key, Flags, 0) of
	    I when is_integer(I) -> 
		radio_alt(I, Qs0, Def, Flags);
	    _ -> 
		[{alt,Def,Prompt,Val,Flags} || {Prompt,Val} <- Qs0]
	end,
    {FrameType,Qs,proplists:delete(key,Flags)}.

radio_alt(I, [{Prompt,Val}|T], Def, Flags) ->
    [{key_alt,{I,Def},Prompt,Val,Flags}|radio_alt(I-1, T, Def, Flags)];
radio_alt(_I, [], _Def, _Flags) -> [].

mktree_leaf(Handler, State, Minimized, W, H, I, Flags) ->
    #fi{handler=Handler,state=State,minimized=Minimized,
	key=proplists:get_value(key, Flags, 0),
	index=I,
	hook=proplists:get_value(hook, Flags),
	stretch=case proplists:lookup(stretch, Flags) of
		    none -> 0;
		    {stretch,S} when is_integer(S), S >= 0 -> S
		end,
	flags=Flags,
	extra=#leaf{w=W,h=H}}.

mktree_priv(Fi, Sto, I, Priv) -> {Fi, gb_trees:insert(-I, Priv, Sto), I+1}.

mktree_container(Qs, Sto0, I0, Flags, Type) ->
    {Fields,Sto1,I} = mktree_container_1(Qs, Sto0, I0+1, []),
    Title = proplists:get_value(title, Flags),
    Minimized = proplists:get_value(minimized, Flags),
    State = case {Title,Minimized} of
		{undefined,_} -> inert;
		{_,undefined} -> inert;
		_ -> enabled
	    end,
    Key = proplists:get_value(key, Flags, 0),
    Sto2 = gb_trees:enter(var(Key, I0), Minimized, Sto1),
    Sto = gb_trees:insert(-I0, Type, Sto2),
    {#fi{handler=fun frame_event/3,
	 key=Key,
	 state=State,
	 minimized=Minimized,
	 index=I0,
	 hook=proplists:get_value(hook, Flags),
	 flags=Flags,
	 extra=#container{type=Type,fields=Fields}},
     Sto,
     I}.

mktree_container_1([], Sto, I, R) ->
    {list_to_tuple(reverse(R)),Sto,I};
mktree_container_1([Q|Qs], Sto0, I0, R) ->
    {Fi,Sto,I} = mktree(Q, Sto0, I0),
    mktree_container_1(Qs, Sto, I, [Fi|R]).

-record(oframe, {style,				%menu|buttons
		 w,h,				%header size
		 titles}).			%tuple() of list()

mktree_oframe(Qs, Def, Sto0, I0, Flags) when is_integer(Def), Def >= 1 ->
    {Titles,Fields,Sto1,I} = mktree_oframe_1(Qs, Sto0, I0+1, [], []),
    FieldsTuple = list_to_tuple(Fields),
    if  Def =< tuple_size(FieldsTuple) ->
	    Style = proplists:get_value(style, Flags, menu),
	    Key = proplists:get_value(key, Flags, 0),
	    Cw = ?CHAR_WIDTH,
	    {W,H} = 
		case Style of
		    menu -> 
			{10 + 2*Cw +
			 lists:foldl(
			   fun (Title, Width) ->
				   max(wings_text:width(Title), Width)
			   end, 0, Titles),
			 ?LINE_HEIGHT+10};
		    buttons -> 
			{lists:foldl(
			   fun (Title, Width) ->
				   2*Cw+Width+wings_text:width(Title)
			   end, 10+2*Cw, Titles),
			 ?LINE_HEIGHT+10}
		end,
	    Sto2 = gb_trees:enter(var(Key, I0), Def, Sto1),
	    Sto = gb_trees:insert(-I0, #oframe{style=Style,
					       w=W,h=H,
					       titles=list_to_tuple(Titles)}, 
				  Sto2),
	    {#fi{handler=fun oframe_event/3,
		 key=Key,
		 state=enabled,
		 index=I0,
		 hook=proplists:get_value(hook, Flags),
		 flags=Flags,
		 extra=#container{type=oframe,fields=FieldsTuple,selected=Def}},
	     Sto,
	     I}
    end.

mktree_oframe_1([], Sto, I, T, R) ->
    {reverse(T),reverse(R),Sto,I};
mktree_oframe_1([{Title,Q}|Qs], Sto0, I0, T, R) when is_list(Title) ->
    {Fi,Sto,I} = mktree(Q, Sto0, I0),
    mktree_oframe_1(Qs, Sto, I, [Title|T], [Fi|R]).

-ifdef(DEBUG).
%%
%% Dump a dialog tree on stdout
%%

dmptree(Fi) -> 
    io:format("-begin(dmptree/1).~n"),
    case catch dmptree(Fi, "") of
	ok -> 
	    io:format("-end(dmptree/1).~n");
	Other ->
	    io:format("-end(dmptree/1, ~p).~n", [Other])
    end.
    
dmptree(#fi{key=Key,index=Index,state=State,minimized=Minimized,flags=Flags,
	       x=X,y=Y,w=W,h=H,stretch=Stretch,extra=Extra}, Fill) ->
    io:format("~s#fi{key=~p,index=~p,flags=~p,x=~p,y=~p,w=~p,h=~p,~n"
	      "~s    state=~p,minimized=~p,stretch=~p",
	      [Fill,Key,Index,Flags,X,Y,W,H,Fill,State,Minimized,Stretch]),
    dmptree_1(Extra, Fill).

dmptree(Fields, Fill, I) when I =< tuple_size(Fields) ->
    dmptree(element(I, Fields), Fill),
    dmptree(Fields, Fill, I+1);
dmptree(_Fields, _Fill, _I) -> ok.

dmptree_1(#container{type=Type,x=X,y=Y,w=W,h=H,
		     fields=Fields,selected=Selected,
		     stretch=Stretch,w0=W0,h0=H0}, Fill) ->
    io:format("}.~n~s  #container{type=~p,selected=~p,"
	      "x=~p,y=~p,w=~p,h=~p,stretch=~p,w0=~p,h0=~p}~n",
	      [Fill,Type,Selected,X,Y,W,H,Stretch,W0,H0]),
    dmptree(Fields, "  "++Fill, 1);
dmptree_1(#leaf{w=W,h=H}, _Fill) -> 
    io:format("#leaf{w=~p,h=~p}}.~n", [W,H]).

-endif.

%%
%% Layout the dialog, i.e calculate positions and sizes for all fields.
%% Container frames take their size from their children, sum in one
%% direction and max in other, depending on vframe/hframe/oframe.
%%
%% Note! The #container.fields field is a reversed list after this pass.

layout(Fi, Sto) -> 
    layout_propagate(layout(Fi, Sto, 0, 0)).

layout(Fi=#fi{key=Key,index=I,hook=Hook,minimized=Minimized0,
	      extra=#leaf{w=W,h=H}}, Sto, X, Y) ->
    Minimized = case hook(Hook, is_minimized, [var(Key, I),I,Sto]) of
		    keep -> Minimized0;
		    Minimized1 -> Minimized1
		end,
    case Minimized of
	true -> Fi#fi{x=X,y=Y,w=0,h=0,minimized=true};
	_ -> Fi#fi{x=X,y=Y,w=W,h=H,minimized=Minimized}
    end;
layout(Fi=#fi{key=Key,index=Index,
	      extra=Container=#container{type=oframe,fields=Fields0}}, 
       Sto, X0, Y0) ->
    #oframe{w=Wt,h=Ht} = gb_trees:get(-Index, Sto),
    Selected = gb_trees:get(var(Key, Index), Sto),
    Pad = 10,
    X1 = X0+Pad,
    Y1 = Y0+Ht,
    {Fields,X2,Y2,Stretch} = layout_container(oframe, Fields0, Sto, X1, Y1),
    Wi = max(Wt, X2-X1),
    Hi = Y2-Y1,
    Wo = Pad+Wi+Pad,
    Ho = Ht + Hi + Pad,
    Fi#fi{x=X0,y=Y0,w=Wo,h=Ho,
	  extra=Container#container{x=X1,y=Y1,w=Wi,h=Hi,w0=Wo,h0=Ho,
				    fields=Fields,
				    selected=Selected,
				    stretch=Stretch}};
layout(Fi=#fi{key=Key,index=Index,flags=Flags,hook=Hook,
	      extra=Container=#container{type=Type,fields=Fields0}}, 
       Sto, X0, Y0) when Type =:= hframe; Type =:= vframe ->
    Var = var(Key, Index),
    Minimized0 = gb_trees:get(Var, Sto),
    Minimized = case hook(Hook, is_minimized, [Var,Index,Sto]) of
		    keep -> Minimized0;
		    Minimized1 -> Minimized1
		end,
    Title = proplists:get_value(title, Flags),
    {X1,Y1} = if Title =:= undefined -> {X0,Y0};
		 true -> {X0+10,Y0+?LINE_HEIGHT} end,
    {Fields,X2,Y2,Stretch} = layout_container(Type, Fields0, Sto, X1, Y1),
    Wi = if Title =:= undefined -> 
		 if Minimized =:= true -> 0;
		    true -> X2-X1 end;
	    true -> max(3*?CHAR_WIDTH+wings_text:width(Title), X2-X1) end,
    Hi = Y2-Y1,
    Wo = 2*(X1-X0)+Wi,
    Ho = Y1-Y0 +
	if Minimized =:= true -> 0; true -> Hi end +
	if Title =:= undefined -> 0; true -> 10 end,
    Fi#fi{x=X0,y=Y0,w=Wo,h=Ho,minimized=Minimized,
	  extra=Container#container{x=X1,y=Y1,w=Wi,h=Hi,w0=Wo,h0=Ho,
				    fields=Fields,stretch=Stretch}}.

layout_container(vframe, Fields, Sto, X, Y) -> 
    layout_vframe(1, Fields, Sto, X, Y, 0, [], 0);
layout_container(hframe, Fields, Sto, X, Y) -> 
    layout_hframe(1, Fields, Sto, X, Y, 0, [], 0);
layout_container(oframe, Fields, Sto, X, Y) -> 
    layout_oframe(1, Fields, Sto, X, Y, 0, 0, [], 0).

layout_vframe(I, Fields, Sto, X0, Y0, W0, R, S) when I =< tuple_size(Fields) ->
    Fi = #fi{x=X0,y=Y0,w=W,h=H,stretch=Stretch} = 
	layout(element(I, Fields), Sto, X0, Y0),
    layout_vframe(I+1, Fields, Sto, X0, Y0+H, max(W, W0), [Fi|R], S+Stretch);
layout_vframe(_I, _Fields, _Sto, X, Y, W, R, S) ->
    {R,X+W,Y,S}.

layout_hframe(I, Fields, Sto, X0, Y0, H0, R, S) when I =< tuple_size(Fields) ->
    Fi = #fi{x=X0,y=Y0,w=W,h=H,stretch=Stretch} = 
	layout(element(I, Fields), Sto, X0, Y0),
    Ws = if W =/= 0, I < tuple_size(Fields) -> ?HFRAME_SPACING;
	    true -> 0
	 end,
    layout_hframe(I+1, Fields, Sto, X0+Ws+W, Y0, max(H, H0), 
		  [Fi|R], S+Stretch);
layout_hframe(_I, _Fields, _Sto, X, Y, H, R, S) ->
    {R,X,Y+H, S}.

layout_oframe(I, Fields, Sto, X, Y, W0, H0, R, S) when I =< tuple_size(Fields) ->
    Fi = #fi{x=X,y=Y,w=W,h=H,stretch=Stretch} = 
	layout(element(I, Fields), Sto, X, Y),
    layout_oframe(I+1, Fields, Sto, X, Y, max(W0, W), max(H0, H), 
		  [Fi|R], S+Stretch);
layout_oframe(_I, _Fields, _Sto, X, Y, W, H, R, S) ->
    {R,X+W,Y+H,S}.
    


%% Propagate the resulting size of container frames to its children.
%%
%% Convert the #container.fields field back to tuple.

layout_propagate(Fi=#fi{extra=Container=#container{
				type=vframe,w=W,fields=Fields}}) ->
    Fi#fi{extra=Container#container{
		  fields=layout_propagate_vframe(Fields, W, [])}};
layout_propagate(Fi=#fi{w=W,
			extra=Container=#container{
				type=hframe,h=H,w=Wi,w0=W0,stretch=Stretch,
				fields=Fields}}) ->
    {Dx,Ds,S} = 
	if Stretch =:= 0 -> {0,0,0}; 
	   true -> 
		Pad = W - W0,
		{Pad,Pad div Stretch,Pad rem Stretch}
	end,
    Fi#fi{extra=Container#container{
		  w=Wi+Dx,
		  fields=layout_propagate_hframe(Fields, H, Dx, Ds, S, [])}};
layout_propagate(Fi=#fi{extra=Container=#container{
				type=oframe,w=W,h=H,fields=Fields}}) ->
    Fi#fi{extra=Container#container{
		  fields=layout_propagate_oframe(Fields, W, H, [])}};
layout_propagate(Fi=#fi{}) ->
    Fi.

layout_propagate_vframe([Fi|Fis], W, R) ->
    layout_propagate_vframe(Fis, W, [layout_propagate(Fi#fi{w=W})|R]);
layout_propagate_vframe([], _W, R) ->
    list_to_tuple(R).

layout_propagate_hframe([#fi{x=X,stretch=0}=Fi|Fis], H, Dx, Ds, S, R) ->
    layout_propagate_hframe(Fis, H, Dx, Ds, S,
			    [layout_propagate(Fi#fi{x=X+Dx,h=H})|R]);
layout_propagate_hframe([#fi{x=X,w=W,stretch=Stretch}=Fi|Fis], 
			H, Dx0, Ds, S, R) ->
    Dw = min(Stretch, S)*(Ds + 1) + max(Stretch-S, 0)*Ds,
    Dx = Dx0-Dw,
    layout_propagate_hframe(Fis, H, Dx, Ds, max(S-Stretch, 0),
			    [layout_propagate(Fi#fi{x=X+Dx,w=W+Dw,h=H})|R]);
layout_propagate_hframe([], _H, _Pad, _Ds, _S, R) ->
    list_to_tuple(R).

layout_propagate_oframe([Fi|Fis], W, H, R) ->
    layout_propagate_oframe(Fis, W, H, [layout_propagate(Fi#fi{w=W,h=H})|R]);
layout_propagate_oframe([], _W, _H, R) ->
    list_to_tuple(R).

%%
%% Create a tuple of the focusable field indexes
%%

focusable(Fi) -> 
    T = list_to_tuple(reverse(focusable_1(Fi, []))),
    ?DEBUG_DISPLAY(tree, T),
    T.
focusable_1(#fi{state=disabled,extra=#container{}}, R) ->
    R;
focusable_1(#fi{index=Index,state=State,minimized=Minimized,
		extra=#container{fields=Fields,selected=Selected},flags=Flags}, R0) ->
    NoFocus = lists:member(no_focus, Flags),
    R = if
        NoFocus -> R0;
        State =:= enabled -> [Index|R0];
        true -> R0 end,
    case Minimized of
	true -> R;
	_ ->
	    case Selected of
		undefined -> focusable_2(1, Fields, R);
		_ -> focusable_1(element(Selected, Fields), R)
	    end
    end;
focusable_1(#fi{minimized=true,extra=#leaf{}}, R) -> R;
focusable_1(#fi{index=Index,state=enabled,extra=#leaf{},flags=Flags}, R) ->
    NoFocus = lists:member(no_focus, Flags),
    if NoFocus -> R;
       true -> [Index|R]
    end;
focusable_1(#fi{extra=#leaf{}}, R) -> R.
    
focusable_2(I, Fields, R) when I =< tuple_size(Fields) ->
    focusable_2(I+1, Fields, focusable_1(element(I, Fields), R));
focusable_2(_I, _Fields, R) -> R.

%% Mark all siblings (that can be minimized) of a container to be minimized
%%

minimize_siblings([#fi{index=Index},
		   #fi{extra=#container{fields=Fields}}|_], Sto) ->
    minimize_siblings(1, Fields, Index, Sto);
minimize_siblings(_Path, Sto) -> Sto.

minimize_siblings(I, Fields, Index, Sto0) when I =< tuple_size(Fields) ->
    case element(I, Fields) of
	#fi{key=Key,index=Ix,hook=Hook,
	    state=enabled,minimized=false,flags=Flags,
	    extra=#container{}} 
	when Ix =/= Index -> 
	    case hook(Hook, update, [var(Key, Ix),Ix,true,Sto0,Flags]) of
		{store,Sto} -> minimize_siblings(I+1, Fields, Index, Sto);
		{done,Sto} -> minimize_siblings(I+1, Fields, Index, Sto);
		{layout,Sto} -> minimize_siblings(I+1, Fields, Index, Sto);
		_ -> minimize_siblings(I+1, Fields, Index, Sto0)
	    end;
	#fi{} -> minimize_siblings(I+1, Fields, Index, Sto0)
    end;
minimize_siblings(_I, _Fields, _Index, Sto) -> Sto.



%%
%% Hframe and Vframe
%%

frame_event({redraw,Active,DisEnabled}, [Fi|_], _Store) ->
    frame_redraw(Active, DisEnabled, Fi);
frame_event(#mousemotion{x=Xm,y=Ym,state=Bst},
	    [#fi{key=Key,index=I,state=enabled,x=X,y=Y}|_],
	    Store) 
  when (Bst band ?BUTTON_MASK) =:= 0 ->
    case inside(Xm, Ym, X, Y+3, 10, ?CHAR_HEIGHT) of
	true ->
	    Minimized = gb_trees:get(var(Key, I), Store),
	    case Minimized of
		true ->
		    wings_msg:button(
		      ?__(1,"Expand this frame; collapse other frames"),
		      "",
		      ?__(2,"Expand this frame"));
		false ->
		    Msg = ?__(3,"Collapse this frame"),
		    wings_msg:button(Msg, "", Msg);
		undefined ->
		    wings_wm:message("")
	    end;
	false ->
	    wings_wm:message("")
    end,
    keep;
frame_event(#mousebutton{x=Xb,y=Yb,button=Button,state=?SDL_RELEASED}, 
	    Path=[#fi{x=X0,y=Y0}|_], 
	    Store) ->
    X = X0,
    Y = Y0+3,
    case inside(Xb, Yb, X, Y, 10, ?CHAR_HEIGHT) of
	true -> 
	    case Button of
		1 -> frame_event({key,$\s,0,$\s}, Path, Store);
		2 -> keep;
		3 -> frame_event({key,$\s,?SHIFT_BITS,$\s}, Path, Store)
	    end;
	false -> keep
    end;
frame_event({key,_,Mod,$\s}, 
	    Path=[#fi{key=Key,index=I,hook=Hook,flags=Flags,
		      minimized=Minimized}|_], 
	    Store0) ->
    case Minimized of
	undefined -> keep;
	true ->
	    Store1 = 
		if  ?IS_SHIFTED(Mod) -> Store0;
		    true -> minimize_siblings(Path, Store0)
		end,
	    hook(Hook, update, [var(Key, I),I,false,Store1,Flags]);
	false -> 
	    hook(Hook, update, [var(Key, I),I,true,Store0,Flags])
    end;
frame_event(value, [#fi{key=Key,index=I,flags=Flags}|_], Store) ->
    {value,(gb_trees:get(var(Key, I), Store) 
	    xor proplists:get_bool(invert, Flags))};
frame_event(_Ev, _Path, _Store) -> keep.

frame_redraw(Active, DisEnabled, #fi{flags=Flags}=Fi) ->
    Title = proplists:get_value(title, Flags),
    if  Title =:= undefined -> keep;
	true -> frame_redraw_1(Active, DisEnabled, Fi, Title) 
    end.

frame_redraw_1(Active, DisEnabled, 
	       #fi{x=X0,y=Y0,w=W0,h=H0,minimized=Minimized,flags=Flags}, 
	       Title) ->
    Cw = ?CHAR_WIDTH,
    Ch = ?CHAR_HEIGHT,
    Y = Y0 + Ch div 2 + 3,
    H = H0 - (Y-Y0) - 4,
    ColLow = color4_lowlight(),
    ColHigh = color4_highlight(),
    TextPos = X0 + 10 + 2*Cw,
    blend(fun(Col) ->
		  gl:'begin'(?GL_LINES),
		  hline(X0, Y, W0, ColLow, ColHigh),
		  if  Minimized =/= true ->
			  hline(X0, Y+H-1, W0-1, ColLow, ColHigh),
			  vline(X0+W0-2, Y, H, ColLow, ColHigh),
			  vline(X0, Y+1, H-2, ColLow, ColHigh);
		      true -> ok
		  end,
		  gl:'end'(),
		  gl:color4fv(Col),
		  gl:rectf(TextPos-Cw, Y-1,
			   TextPos+wings_text:width(Title)+Cw, Y+2)
	  end),
    ColFg = color3_text(),
    gl:color3fv(ColFg),
    if  Minimized =/= undefined ->
	    case proplists:get_bool(checkbox, Flags) of
		true ->
		    Val = Minimized xor proplists:get_bool(invert, Flags),
		    draw_checkbox(Active, X0, Y0, Title, Val, DisEnabled);
		false ->
		    wings_io:text_at(TextPos, Y0+Ch, Title),
		    %% Draw button
		    blend(fun(Col) ->
				  case DisEnabled of
				      enabled ->
					  wings_io:gradient_border(
					    X0, Y0+3,
					    10, Ch,
					    Col, ColFg, Active);
				      _ ->
					  wings_io:border(
					    X0, Y0+3, 
					    10, Ch,
					    Col, ColFg)
				  end
			  end),
		    %% Draw +/- symbol
		    gl:color3fv(ColFg),
		    gl:rectf(X0+2, Y, X0+9, Y+2),
		    case Minimized of
			true -> gl:rectf(X0+4, Y-2, X0+6, Y+4);
			false -> ok
		    end
	    end;
	true -> 
	    wings_io:text_at(TextPos, Y0+Ch, Title)
    end,
    keep.

hline(X0, Y0, W, ColLow, ColHigh) ->
    X = X0 + 0.5,
    Y = Y0 + 0.5,
    gl:color4fv(ColLow),
    gl:vertex2f(X, Y),
    gl:vertex2f(X+W, Y),
    gl:color4fv(ColHigh),
    gl:vertex2f(X, Y+1),
    gl:vertex2f(X+W, Y+1).

vline(X0, Y0, H, ColLow, ColHigh) ->
    X = X0 + 0.5,
    Y = Y0 + 0.5,
    gl:color4fv(ColLow),
    gl:vertex2f(X, Y),
    gl:vertex2f(X, Y+H),
    gl:color4fv(ColHigh),
    gl:vertex2f(X+1, Y),
    gl:vertex2f(X+1, Y+H).



%%
%% Oframe
%%

oframe_event({redraw,Active,_DisEnabled}, [Fi=#fi{index=I}|_], Store) ->
    Oframe = gb_trees:get(-I, Store),
    oframe_redraw(Active, Fi, Oframe);
oframe_event(value, [#fi{key=Key,index=I}|_], Store) ->
    {value,gb_trees:get(var(Key, I), Store)};
oframe_event(#mousemotion{state=Bst}, _Path, _Store)
  when (Bst band ?BUTTON_MASK) =:= 0 ->
    wings_wm:message(""),
    keep;
oframe_event(#mousebutton{x=Xb,button=1,state=?SDL_PRESSED}, 
	     Path=[#fi{x=X,w=W,key=Key,index=I,hook=Hook,flags=Flags}|_], 
	     Sto0) ->
    case gb_trees:get(-I, Sto0) of
	#oframe{style=buttons,titles=Titles} ->
	    #oframe{w=Wt} = gb_trees:get(-I, Sto0),
	    Xt = oframe_title_xpos(X, W, Wt),
	    case oframe_which_tab(Xt, Xb, Titles) of
		undefined -> keep;
		Val -> hook(Hook, update, [var(Key, I),I,Val,Sto0,Flags])
	    end;
	#oframe{style=menu} ->
	    oframe_event({key,$\s,0,$\s}, Path, Sto0)
    end;
oframe_event({popup_result,Val}, 
	     [#fi{index=I,key=Key,hook=Hook,flags=Flags}|_], Sto0) ->
    hook(Hook, update, [var(Key, I),I,Val,Sto0,Flags]);
oframe_event({key,_,0,$\s}, [#fi{x=X,y=Y,key=Key,index=I}|_], Sto) ->
    case gb_trees:get(-I, Sto) of
	#oframe{w=W,style=menu,titles=Titles} ->
	    Menu = oframe_menu(Titles),
	    menu_popup(X+10, Y, W, Menu, gb_trees:get(var(Key, I), Sto), []);
	#oframe{style=buttons} -> keep
    end;
oframe_event({key,_,_,$\^B}, Path, Sto) -> % Ctrl-B
    oframe_step(-1, Path, Sto);
oframe_event({key,_,_,$\^F}, Path, Sto) ->
    oframe_step(+1, Path, Sto);
oframe_event(_Ev, _Path, _Store) -> keep.

oframe_menu(Titles) -> oframe_menu(Titles, 1).

oframe_menu(Titles, N) when N =< tuple_size(Titles) -> 
    [{element(N, Titles),N,[]}|oframe_menu(Titles, N+1)];
oframe_menu(_Titles, _N) -> [].

oframe_redraw(Active,
	      #fi{x=X0,y=Y0,w=W0,h=H0,extra=#container{selected=Selected}},
	      #oframe{style=menu,w=Wt,h=Ht,titles=Titles}) ->
    Y = Y0+((Ht-10+4) div 2),
    H = H0-(Y-Y0+5),
    ColLow = color4_lowlight(),
    ColHigh = color4_highlight(),
    blend(fun(_Col) ->
		  gl:'begin'(?GL_LINES),
 		  hline(X0, Y, W0, ColLow, ColHigh),
		  hline(X0, Y+H-2, W0, ColLow, ColHigh),
		  vline(X0, Y+1, H-4, ColLow, ColHigh),
		  vline(X0+W0, Y+1, H-4, ColLow, ColHigh),
		  gl:'end'()
	  end),
    Title = element(Selected, Titles),
    menu_draw(Active, 10, Y0, Wt, Ht-10+4, Title, enabled);
oframe_redraw(Active,
	      #fi{x=X0,y=Y0,w=W0,h=H0,extra=#container{selected=Selected}},
	      #oframe{style=buttons,w=Wt,h=Ht,titles=Titles}) ->
    Y = Y0+((Ht-10+4) div 2),
    H = H0-(Y-Y0+5),
    ColLow = color4_lowlight(),
    ColHigh = color4_highlight(),
    blend(fun(_Col) ->
		  gl:'begin'(?GL_LINES),
 		  hline(X0, Y, W0, ColLow, ColHigh),
		  hline(X0, Y+H-2, W0, ColLow, ColHigh),
		  vline(X0, Y+1, H-4, ColLow, ColHigh),
		  vline(X0+W0, Y+1, H-4, ColLow, ColHigh),
		  gl:'end'()
	  end),
    Xt = oframe_title_xpos(X0, W0, Wt),
    oframe_redraw_titles(Active, Xt, Y0, Ht-5, Selected, Titles),
    keep.

oframe_title_xpos(X0, W0, Wt) -> X0 + ((W0-(Wt-10)) div 2).

oframe_which_tab(X0, Xb, Titles) when Xb >= X0 -> 
    oframe_which_tab(X0, Xb, Titles, ?CHAR_WIDTH, 1);
oframe_which_tab(_X0, _Xb, _Titles) -> undefined.

oframe_which_tab(X0, Xb, Titles, Cw, I) when I =< tuple_size(Titles) ->
    W = Cw + wings_text:width(element(I, Titles)) + Cw,
    if  Xb < X0+W -> I;
	true -> oframe_which_tab(X0+W, Xb, Titles, Cw, I+1)
    end;
oframe_which_tab(_, _, _, _, _) -> undefined.

oframe_redraw_titles(Active, X, Y, H, Selected, Titles) -> 
    Cw = ?CHAR_WIDTH,
    ColText = color3_text(),
    ColPane = color3(),
    ColHigh = color3_high(),
    oframe_redraw_titles(Active, X, Y, H, Selected, Titles, 
			 Cw, ColText, ColPane, ColHigh, 1).

oframe_redraw_titles(Active, X, Y, H, Selected, Titles, 
		     Cw, ColText, ColPane, ColHigh, I)
  when I =< tuple_size(Titles) ->
    Title = element(I, Titles),
    W = wings_text:width(Title),
    X2 = X+Cw+W+Cw,
    Result = 
	%% Draw active tab last
	if I =:= Selected ->
		R = oframe_redraw_titles(Active, X2, Y, H, Selected, Titles,
					 Cw, ColText, ColPane, ColHigh, I+1),
		wings_io:gradient_border(X, Y, X2-X, H, 
					 ColHigh, ColText, Active),
		R;
	   true -> 
		wings_io:border(X, Y, X2-X, H, ColPane, ColText),
		keep
	end,
    gl:color3fv(ColText),
    wings_io:text_at(X+Cw, Y+H-4, Title),
    %% Draw active tab last
    if I =/= Selected ->
	    oframe_redraw_titles(Active, X2, Y, H, Selected, Titles, 
				 Cw, ColText, ColPane, ColHigh, I+1);
       true -> Result
    end;
oframe_redraw_titles(_Active, _X, _Y, _H, _Selected, _Titles, 
		     _Cw, _ColText, _ColPane, _ColHigh, _I) -> keep.

oframe_step(Step, [#fi{key=Key,index=I,hook=Hook,flags=Flags,
		       extra=#container{fields=Fields}}|_], Sto0) ->
    case gb_trees:get(-I, Sto0) of
	#oframe{style=buttons} ->
	    Var = var(Key, I),
	    Val = gb_trees:get(Var, Sto0) + Step,
	    if 1 =< Val, Val =< tuple_size(Fields) -> 
		    hook(Hook, update, [Var,I,Val,Sto0,Flags]);
	       true -> keep
	    end;
	#oframe{style=menu} -> keep
    end.


%%%
%%% Separator.
%%%

-record(separator, {}).

mktree_separator(Sto, I, Flags) ->
    Fi = mktree_leaf(fun separator_event/3, inert, undefined,
		     4*?CHAR_WIDTH, 10, I, Flags),
    mktree_priv(Fi, Sto, I, #separator{}).

separator_event({redraw,_Active,_DisEnabled}, [Fi|_], _Sto) -> 
    separator_draw(Fi);
separator_event(_Ev, _Path, _Sto) -> keep.

separator_draw(#fi{x=X,y=Y,w=W}) ->
    ?CHECK_ERROR(),
    LeftX = X + 0.5,
    RightX = X + W + 0.5,
    UpperY = Y + 5.5,
    {R,G,B} = color3_disabled(),
    {_,_,_,A} = color4(),
    FgColor = {R,G,B,A},
    wings_io:blend(FgColor,
		   fun(Col) ->
			   gl:lineWidth(1),
			   gl:'begin'(?GL_LINES),
			   gl:color4fv(Col),
			   gl:vertex2f(LeftX, UpperY),
			   gl:vertex2f(RightX, UpperY),
			   gl:'end'()
		   end),
    gl:color3b(0, 0, 0),
    ?CHECK_ERROR(),
    keep.



%%%
%%% Checkbox
%%%

-define(CB_SIZE, 10).

-record(cb,
	{label,
	 labelw,				%Width of label in pixels.
	 spacew,				%Width of a space character.
	 val
	}).

mktree_checkbox(Label, Val, Sto, I, Flags) ->
    LabelWidth = wings_text:width(Label),
    SpaceWidth = wings_text:width(" "),
    Fi = #fi{key=Key} =
	mktree_leaf(fun cb_event/3, enabled, undefined,
		    LabelWidth+SpaceWidth+?CB_SIZE, ?LINE_HEIGHT+2,
		    I, Flags),
    Cb = #cb{label=Label,val=Val,labelw=LabelWidth,spacew=SpaceWidth},
    mktree_priv(Fi, gb_trees:enter(var(Key, I), Val, Sto), I, Cb).

cb_event(value, [#fi{key=Key,index=I}|_], Store) ->
    {value,gb_trees:get(var(Key, I), Store)};
cb_event({redraw,Active,DisEnabled}, [Fi=#fi{key=Key,index=I}|_], 
	 Store) ->
    Cb = gb_trees:get(-I, Store),
    Val = gb_trees:get(var(Key, I), Store),
    cb_draw(Active, Fi, Cb, Val, DisEnabled);
cb_event(#mousebutton{x=Xb,state=?SDL_PRESSED,button=1}, 
	 Path=[#fi{x=X,index=I}|_], Store) ->
    #cb{labelw=LblW,spacew=SpaceW} = gb_trees:get(-I, Store),
    if
	Xb-X < LblW+4*SpaceW ->
	    cb_event({key,$\s,0,$\s}, Path, Store);
	true -> keep
    end;
cb_event({key,_,_,$\s}, 
	 [#fi{key=Key,index=I,hook=Hook,flags=Flags}|_], Store) ->
    Var = var(Key, I),
    Val = gb_trees:get(Var, Store),
    hook(Hook, update, [Var,I,not Val,Store,Flags]);
cb_event(_Ev, _Path, _Store) -> keep.

cb_draw(Active, #fi{x=X,y=Y}, #cb{label=Label}, Val, DisEnabled) ->
    draw_checkbox(Active, X, Y, Label, Val, DisEnabled),
    keep.

draw_checkbox(Active, X, Y0, Label, Val, DisEnabled) ->
    wings_io:sunken_gradient(X, Y0+?CHAR_HEIGHT-?CB_SIZE, ?CB_SIZE, ?CB_SIZE,
			     case DisEnabled of
				 enabled -> color3_high();
				 _ -> color3()
			     end, color4(), Active),
    FgColor = case DisEnabled of
		  enabled -> color3_text();
		  _ -> color3_disabled()
	      end,
    gl:color3fv(FgColor),
    Y = Y0+?CHAR_HEIGHT,
    case Val of
	false -> ok;
	true -> wings_io:text_at(X+2, Y-1, [crossmark])
    end,
    wings_io:text_at(X+round(1.8*?CB_SIZE), Y, Label),
    gl:color3b(0, 0, 0).



%%%
%%% Radio button
%%%

-record(rb,
	{val,
	 label,
	 labelw,			    %Width of label in pixels.
	 spacew				  %Width of a space character.
	}).

mktree_radiobutton(Def, Label, Val, Sto0, I, Flags) ->
    LabelWidth = wings_text:width(Label),
    SpaceWidth = wings_text:width(" "),
    Rb = #rb{val=Val,label=Label,
	     labelw=LabelWidth,spacew=SpaceWidth},
    Fi = #fi{key=Key} = 
	mktree_leaf(fun rb_event/3, enabled, undefined,
		    LabelWidth+2*SpaceWidth, ?LINE_HEIGHT+2, I, Flags),
    Sto = if Val =:= Def -> gb_trees:enter(var(Key, I), Val, Sto0);
	     true -> Sto0 end,
    mktree_priv(Fi, Sto, I, Rb).

rb_event({redraw,Active,DisEnabled}, [Fi=#fi{key=Key,index=I}|_], 
	 Store) ->
    Rb = gb_trees:get(-I, Store),
    Val = gb_trees:get(var(Key, I), Store),
    rb_draw(Active, Fi, Rb, Val, DisEnabled);
rb_event(value, [#fi{key=Key,index=I}|_], Store) ->
    #rb{val=Val} = gb_trees:get(-I, Store),
    case gb_trees:get(var(Key, I), Store) of
	Val -> {value,Val};
	_ -> none
    end;
rb_event({key,_,_,$\s}, [Fi=#fi{index=I}|_], Store) ->
    rb_set(Fi, gb_trees:get(-I, Store), Store);
rb_event(#mousebutton{x=Xb,state=?SDL_RELEASED,button=1}, 
	 [Fi=#fi{x=X,index=I}|_], Store) ->
    #rb{labelw=LblW,spacew=SpaceW} = Rb = gb_trees:get(-I, Store),
    if
	Xb-X < LblW+4*SpaceW -> rb_set(Fi, Rb, Store);
	true -> keep
    end;
rb_event(_Ev, _Fi, _Store) -> keep.

rb_draw(Active, #fi{x=X,y=Y0}, #rb{label=Label,val=Val}, Common, DisEnabled) ->
    FgColor = case DisEnabled of
		  enabled -> color3_text();
		  _ -> color3_disabled()
	      end,
    Y = Y0+?CHAR_HEIGHT,
    gl:color3fv(case DisEnabled of
		    enabled -> color3_high();
		    _ -> color3()
		end),
    Fg = <<
	  2#0000000000000000:16,
	  2#0001110000000000:16,
	  2#0011111000000000:16,
	  2#0111111100000000:16,
	  2#0111111100000000:16,
	  2#0111111100000000:16,
	  2#0011111000000000:16,
	  2#0001110000000000:16,
	  2#0000000000000000:16>>,
    gl:rasterPos2i(X, Y),
    gl:bitmap(9, 9, -1, 0, 7, 0, Fg),
    gl:color3fv(FgColor),
    B = case Common of
	    Val ->
	       	<<
	       	 2#0001110000000000:16,
	       	 2#0010001000000000:16,
	       	 2#0100100100000000:16,
	       	 2#1001110010000000:16,
	       	 2#1011111010000000:16,
	       	 2#1001110010000000:16,
	       	 2#0100100100000000:16,
	       	 2#0010001000000000:16,
	       	 2#0001110000000000:16>>;
	    _ ->
	       	<<
	       	 2#0001110000000000:16,
	       	 2#0010001000000000:16,
	       	 2#0100000100000000:16,
	       	 2#1000000010000000:16,
	       	 2#1000000010000000:16,
	       	 2#1000000010000000:16,
	       	 2#0100000100000000:16,
	       	 2#0010001000000000:16,
	       	 2#0001110000000000:16>>
    end,
    gl:rasterPos2i(X, Y),
    gl:bitmap(9, 9, -1, 0, 9, 0, B),
    gl:color3fv(FgColor),
    wings_io:text_at(X+2*?CHAR_WIDTH, Y, Label),
    if
	Active == true ->
	    Border = <<
		      2#0001111100000000:16,
		      2#0011000110000000:16,
		      2#0110000011000000:16,
		      2#1100000001100000:16,
		      2#1100000001100000:16,
		      2#1100000001100000:16,
		      2#1100000001100000:16,
		      2#1100000001100000:16,
		      2#0110000011000000:16,
		      2#0011000110000000:16,
		      2#0001111100000000:16>>,
	    gl:rasterPos2i(X, Y),
	    gl:bitmap(11, 11, 0, 1, 0, 0, Border);
	true -> ok
    end,
    gl:color3b(0, 0, 0),
    keep.

rb_set(#fi{key=Key,index=I,hook=Hook,flags=Flags}, #rb{val=Val}, Store) ->
    hook(Hook, update, [var(Key, I),I,Val,Store,Flags]).

%%%
%%% Menu Box
%%%

-record(menu, {menu}).

mktree_menu(Menu0, Def, Sto, I, Flags) ->
    Menu = [case X of
		{D,V} -> {D,V,[]};
		{_,_,F}=DVF when is_list(F) -> DVF 
	    end || X <- Menu0],
    W = menu_width(Menu, 0) + 4 * ?CHAR_WIDTH,
    Fun = fun menu_event/3,
    Fi = #fi{key=Key} = mktree_leaf(Fun, enabled, undefined,
				    W, ?LINE_HEIGHT+4, I, Flags),
    M = #menu{menu=Menu},
    mktree_priv(Fi, gb_trees:enter(var(Key, I), Def, Sto), I, M).

menu_event({redraw,Active,DisEnabled}, 
	   [#fi{key=Key,index=I,x=X,y=Y,w=W,h=H}|_], Store) ->
    #menu{menu=Menu} = gb_trees:get(-I, Store),
    Val = gb_trees:get(var(Key, I), Store),
    ValStr = [Desc || {Desc,V,_} <- Menu, V =:= Val],
    menu_draw(Active, X, Y, W, H, ValStr, DisEnabled);
menu_event(value, [#fi{key=Key,index=I}|_], Store) ->
    {value,gb_trees:get(var(Key, I), Store)};
menu_event(#mousebutton{state=?SDL_PRESSED,button=1}, Path, Store) ->
    menu_event({key,$\s,0,$\s}, Path, Store);
menu_event({key,_,_,$\s}, 
	   [#fi{x=X,y=Y,w=W,hook=Hook,key=Key,index=I}|_], Store) ->
    #menu{menu=Menu} = gb_trees:get(-I, Store),
    Var = var(Key, I),
    Val = gb_trees:get(Var, Store),
    Disabled = hook(Hook, menu_disabled, [Var, I, Store]),
    menu_popup(X, Y, W, Menu, Val, Disabled);
menu_event({popup_result,Val}, 
	   [#fi{index=I,key=Key,hook=Hook,flags=Flags}|_], Store) ->
    hook(Hook, update, [var(Key, I),I,Val,Store,Flags]);
menu_event(_Ev, _Path, _Store) -> keep.

menu_width([{D,_,_}|T], W0) ->
    case wings_text:width(D) of
	W when W < W0 -> menu_width(T, W0);
	W -> menu_width(T, W)
    end;
menu_width([], W) -> W.

menu_draw(Active, X, Y0, W, H, Text, DisEnabled) ->
    FgColor = case DisEnabled of
		  enabled -> color3_text();
		  _ -> color3_disabled()
	      end,
    blend(fun(Col) ->
		  case DisEnabled of
		      enabled ->
			  wings_io:gradient_border(X, Y0+3, W - ?CHAR_WIDTH,
						   H-2, Col, FgColor, Active);
		      _ ->
			  wings_io:border(X, Y0+3, W - ?CHAR_WIDTH,
					  H-2, Col, FgColor)
		  end
	  end),
    Y = Y0+?CHAR_HEIGHT,
    gl:color3fv(FgColor),
    wings_io:text_at(X+4, Y+3, Text),
    Xr = X + W - round(?CHAR_HEIGHT * 0.75),
    draw_double_arrows(Xr, Y+4),
    gl:color3b(0, 0, 0),
    keep.

draw_double_arrows(X, Y) ->
    Cw = ?CHAR_WIDTH,
    H = ?CHAR_HEIGHT,
    ?CHECK_ERROR(),
    %% Top Triangle
    gl:'begin'(?GL_TRIANGLES),
    gl:vertex2i(X, Y - round(H/2.5)),
    gl:vertex2i(X - Cw, Y - round(H/2.5)),
    %% Tip
    gl:vertex2i(X - Cw div 2, Y - H + H div 3),

    %% Bottom Triangle
    gl:vertex2i(X, Y - round(H/3.5)),
    gl:vertex2i(X - Cw, Y - round(H/3.5)),
    %% Tip
    gl:vertex2i(X - Cw div 2, Y),
    gl:'end'(),
    ?CHECK_ERROR().

%% Menu popup

-record(popup,
	{parent,				%Parent window name.
	 sel,					%Selected element (integer).
	 orig_sel,				%Original selection (integer).
	 menu					%Tuple.
	}).

menu_popup(X0, Y0, W, Menu0, Val, Disabled) ->
    {X1,Y1} = wings_wm:local2global(X0+?HMARGIN, Y0+?VMARGIN),
    Menu1 = [case proplists:get_value(V, Disabled) of
		 undefined -> {Desc,V,false,Flags};
		 true -> {Desc,V,true,Flags};
		 F when is_list(F) -> {Desc,V,true,F++Flags}
	     end || {Desc,V,Flags} <- Menu0],
    case popup_find_index(Menu1, Val) of
	0 -> ok;
	Sel ->
	    Menu = list_to_tuple(Menu1),
	    Mh = tuple_size(Menu)*?LINE_HEIGHT,
	    Ps = #popup{parent=wings_wm:this(),sel=Sel,orig_sel=Sel,menu=Menu},
	    Op = {seq,push,get_popup_event(Ps)},
	    X = X1-2*?CHAR_WIDTH,
	    Y = Y1-2-(Sel-1)*?LINE_HEIGHT,
	    wings_wm:new(menu_popup, {X,Y,highest},
			 {W+2*?CHAR_WIDTH,Mh+10}, Op),
	    wings_wm:grab_focus(menu_popup)
    end,
    keep.

popup_find_index(Menu, Val) ->
    popup_find_index(Menu, Val, 1, 0).

popup_find_index([], _Val, _I, J) -> J;
popup_find_index([{_,Val,false,_}|_], Val, I, _J) -> I;
popup_find_index([{_,_,false,_}|T], Val, I, 0) ->
    popup_find_index(T, Val, I+1, I);
popup_find_index([_|T], Val, I, J) ->
    popup_find_index(T, Val, I+1, J).


get_popup_event(Ps) ->
    {replace,fun(Ev) -> popup_event(Ev, Ps) end}.

popup_event(redraw, Ps) ->
    popup_redraw(Ps);
popup_event(#mousemotion{y=Y}, #popup{menu=Menu,sel=Sel0}=Ps) ->
    case ((Y-2) div ?LINE_HEIGHT)+1 of
	Sel when 1 =< Sel, Sel =< tuple_size(Menu) ->
	    {_,_,Disabled,Flags} = element(Sel, Menu),
	    case proplists:get_value(info, Flags) of
		undefined -> wings_wm:message("");
		Info -> wings_wm:message(Info)
	    end,
	    if  Disabled =:= true -> keep;
		Sel =:= Sel0 -> keep;
		true ->
		    wings_wm:dirty(),
		    get_popup_event(Ps#popup{sel=Sel})
	    end;
	_ ->
	    wings_wm:message(""),
	    keep
    end;
popup_event(#mousebutton{state=?SDL_RELEASED,button=1}, Ps) ->
%%%    wings_wm:message(""),
    popup_key($ , 0, $ , Ps);
popup_event(#keyboard{sym=Sym,mod=Mod,unicode=Unicode}, Ps) ->
    wings_wm:message(""),
    popup_key(Sym, Mod, Unicode, Ps);
popup_event(_Event, _Ps) ->
    keep.

popup_key(?SDLK_TAB, Mod, _Unicode, Ps) when ?IS_SHIFTED(Mod) ->
    popup_key($\^P, Ps);
popup_key(?SDLK_TAB, _Mod, _Unicode, Ps) ->
    popup_key($\^N, Ps);
popup_key(_Sym, Mod, $\t, Ps) when ?IS_SHIFTED(Mod) ->
    popup_key($\^P, Ps);
popup_key(_Sym, _Mod, $\t, Ps) ->
    popup_key($\^N, Ps);
popup_key(?SDLK_UP, _Mod, _Unicode, Ps) ->
    popup_key($\^P, Ps);
popup_key(?SDLK_DOWN, _Mod, _Unicode, Ps) ->
    popup_key($\^N, Ps);
popup_key(?SDLK_KP_ENTER, _Mod, _Unicode, Ps) ->
    popup_key($ , Ps);
popup_key(?SDLK_ESCAPE, _Mod, _Unicode, Ps) ->
    popup_key($\e, Ps);
popup_key(?SDLK_HOME, _Mod, _Unicode, Ps) ->
    popup_key($\^A, Ps);
popup_key(?SDLK_END, _Mod, _Unicode, Ps) ->
    popup_key($\^E, Ps);
popup_key(_Sym, _Mod, $\r, Ps) ->
    popup_key($ , Ps);
popup_key(_Sym, _Mod, Unicode, Ps) ->
    popup_key(Unicode, Ps).

popup_key($\^P, Ps=#popup{sel=Sel}) ->
    case popup_sel(-1, Sel, Ps) of
	Sel -> keep;
	NewSel ->
	    wings_wm:dirty(),
	    get_popup_event(Ps#popup{sel=NewSel})
    end;
popup_key($\^N, Ps=#popup{sel=Sel}) ->
    case popup_sel(+1, Sel, Ps) of
	Sel -> keep;
	NewSel ->
	    wings_wm:dirty(),
	    get_popup_event(Ps#popup{sel=NewSel})
    end;
popup_key($\^A, Ps=#popup{sel=Sel}) ->
    case popup_sel(+1, 0, Ps) of
	Sel -> keep;
	NewSel ->
	    wings_wm:dirty(),
	    get_popup_event(Ps#popup{sel=NewSel})
    end;
popup_key($\^E, Ps=#popup{sel=Sel,menu=Menu}) ->
    case popup_sel(-1, tuple_size(Menu)+1, Ps) of
	Sel -> keep;
	NewSel ->
	    wings_wm:dirty(),
	    get_popup_event(Ps#popup{sel=NewSel})
    end;
popup_key($\e, #popup{parent=Parent,menu=Menu,orig_sel=OrigSel}) ->
    {_,Val,_,_} = element(OrigSel, Menu),
    wings_wm:send(Parent, {popup_result,Val}),
    delete;
popup_key($ , #popup{parent=Parent,menu=Menu,sel=Sel}) -> %Space
    {_,Val,_,_} = element(Sel, Menu),
    wings_wm:send(Parent, {popup_result,Val}),
    delete;
popup_key(_Unicode, _Ps) ->
    keep.

popup_sel(Step, Sel, #popup{sel=PrevSel,menu=Menu}=Ps) ->
    case Sel+Step of
	NewSel when NewSel >= 1, NewSel =< tuple_size(Menu) ->
	    case element(NewSel, Menu) of
		{_,_,false,_} -> NewSel; % not disabled
		_ -> popup_sel(Step, NewSel, Ps)
	    end;
	_ -> PrevSel
    end.

popup_redraw(#popup{sel=Sel,orig_sel=OrigSel,menu=Menu}) ->
    FgColor = color3_text(),
    wings_io:ortho_setup(),
    {_,_,W,H} = wings_wm:viewport(),
    blend(fun(Col) ->
		  wings_io:border(0, 0, W-1, H-1, Col, FgColor)
	  end),
    gl:color3fv(FgColor),
    X = 3*?CHAR_WIDTH-1,
    Y = ?CHAR_HEIGHT+2,
    popup_redraw_1(1, Menu, Sel, W, X, ?CHAR_HEIGHT+2),
    gl:color3fv(FgColor),
    wings_io:text_at(X-10, OrigSel*Y, [crossmark]),
    gl:color3b(0, 0, 0),
    keep.

popup_redraw_1(Sel, Menu, Sel, W, X, Y) ->
    {Desc,_,_,_} = element(Sel, Menu),
    gl:color3fv(color5()),
    gl:recti(X-2, Y+2, X+W-4*?CHAR_WIDTH, Y-?CHAR_HEIGHT+2),
    gl:color3fv(color6()),
    wings_io:text_at(X, Y, Desc),
    popup_redraw_1(Sel+1, Menu, Sel, W, X, Y+?LINE_HEIGHT);
popup_redraw_1(I, Menu, Sel, W, X, Y) when I =< tuple_size(Menu) ->
    {Desc,_,Disabled,_} = element(I, Menu),
    case Disabled of
	true ->
	    gl:color3fv(color3_disabled());
	false ->
	    gl:color3fv(color3_text())
    end,
    wings_io:text_at(X, Y, Desc),
    popup_redraw_1(I+1, Menu, Sel, W, X, Y+?LINE_HEIGHT);
popup_redraw_1(_, _, _, _, _, _) -> keep.



%%%
%%% Buttons
%%%

-record(but,
	{label,					%Textual label.
	 action}).

mktree_button(Action, Sto, I, Flags) -> 
    mktree_button(button_label(Action), Action, Sto, I, Flags).

mktree_button(Label, Action, Sto, I, Flags) ->
    W = case proplists:get_value(width, Flags) of
	      undefined -> max(wings_text:width([$\s,$\s|Label]),
			       wings_text:width(" cancel "));
	    M when is_integer(M), M >= 1 -> (M+2)*?CHAR_WIDTH
	  end,
    Fun = fun button_event/3,
    Fi = #fi{key=Key} = 
	mktree_leaf(Fun, enabled, undefined, W, ?LINE_HEIGHT+2+2, I, Flags),
    %% Trick to not have to change store for 'ok' or 'cancel'
    Val = (Action =:= ok) orelse (Action =:= cancel),
    But = #but{label=Label,action=Action},
    mktree_priv(Fi, gb_trees:enter(var(Key, I), Val, Sto), I, But).

button_label(ok) -> ?__(1,"OK");
button_label(cancel) -> wings_s:cancel();
button_label(Act) -> wings_util:cap(atom_to_list(Act)).
%%button_label(Act) -> wings_util:cap(wings_lang:get_act(Act)). %BUGBUG

button_event({redraw,Active,DisEnabled}, [Fi=#fi{index=I}|_], Store) ->
    button_draw(Active, Fi, gb_trees:get(-I, Store), DisEnabled);
button_event(value, [#fi{key=Key,index=I}|_], Store) ->
    case gb_trees:get(-I, Store) of
	#but{action=done} -> {value,gb_trees:get(var(Key, I), Store)};
	#but{} -> none
    end;
button_event(#mousebutton{x=X,y=Y,state=?SDL_RELEASED,button=1},
	     Path=[#fi{x=Bx,y=By,w=W,h=H}|_], Store)
  when Bx =< X, X =< Bx+W, By =< Y, Y =< By+H ->
    button_event({key,$\s,0,$\s}, Path, Store);
button_event({key,_,Mod,$\r}, Path, Store) ->
    button_event({key,$\s,Mod,$\s}, Path, Store);
button_event({key,_,_,$\s}, 
	     [#fi{key=Key,index=I,hook=Hook,flags=Flags}|_], Store0) ->
    #but{action=Action} = gb_trees:get(-I, Store0),
    case hook(Hook, update, [var(Key, I),I,true,Store0,Flags]) of
	keep -> Action;
	{store,_Store} = Result when Action == keep -> Result;
	{store,Store} -> {Action,Store};
	Result -> Result
    end;
button_event({drop,DropData},[#fi{index=I,flags=Flags}|_], Store) ->
    case proplists:lookup(drop_flags, Flags) of
	none -> keep;
	{drop_flags,DropFlags} ->
	    DropHook = proplists:get_value(hook, DropFlags),
	    DropKey = proplists:get_value(key, DropFlags, 0),
	    {index,DropIndex} = proplists:lookup(index, DropFlags),
	    DropVar = var(DropKey, DropIndex+I),
	    hook(DropHook, update, 
		 [DropVar,DropIndex+I,DropData,Store,DropFlags])
    end;
button_event(_Ev, _Path, _Store) -> keep.

button_draw(Active, #fi{x=X,y=Y0,w=W,h=H}, #but{label=Label}, DisEnabled) ->
    Y = Y0+?CHAR_HEIGHT+2,
    FgColor = case DisEnabled of
		  enabled -> color3_text();
		  _ -> color3_disabled()
	      end,
    blend(fun(Col) ->
		  case DisEnabled of
		      enabled ->
			  wings_io:gradient_border(X, Y0+2, W, H-3,
						   Col, FgColor, Active);
		      _ ->
			  wings_io:border(X, Y0+2, W, H-3, Col, FgColor)
		  end
	  end),
    TextX = X + 2 + (W-wings_text:width(Label)) div 2,
    gl:color3fv(FgColor),
    wings_io:text_at(TextX, Y, Label),
    gl:color3b(0, 0, 0),
    keep.

drop_hook_fun(Flags) ->
    Hook = proplists:get_value(hook, Flags),
    fun (update, {Var,Index,{filename,Filename},Store}) ->
	    hook(Hook, update, 
		 [Var,Index,Filename,Store,Flags]);
	(update, {_Var,_Index,_Val,_Store}) ->
	    keep;
	(_Op, _Args) when Hook =:= undefined ->
	    void;
	(Op, Args) ->
	    Hook(Op, Args)
    end.

browse_hook_fun(Index, Flags) ->
    Ps0 = proplists:get_value(props, Flags),
    Hook = proplists:get_value(hook, Flags),
    Key = proplists:get_value(key, Flags, 0),
    fun(update, {_Var,I,_,Sto}) ->
	    %% This ".junk" trick is to get Name and Dir right 
	    %% even with empty filename.
	    Name0 = gb_trees:get(var(Key, I+Index), Sto)++".junk",
	    Dir = filename:dirname
		    (filename:absname
		     (Name0, wings_pref:get_value(current_directory))),
	    Name = filename:basename(Name0, ".junk"),
	    Ps1 = 
		case proplists:is_defined(directory, Ps0) of
		    true -> Ps0;
		    false -> [{directory,Dir}|Ps0]
		end,
	    Ps =     
		case proplists:is_defined(default_filename, Ps1) of
		    true -> Ps1;
		    false -> [{default_filename,Name}|Ps1]
		end,
	    Parent = wings_wm:this(),
	    F = fun(Filename) ->
			wings_wm:send(Parent, {drop,{filename,Filename}})
		end,
	    DlgType = proplists:get_value(dialog_type, Ps),
	    wings_plugin:call_ui({file,DlgType,Ps,F}),
	    keep;
       (is_disabled, {_,I,Sto}) ->
	    %% This button always has the same disabled state
	    %% as the associated text field.
	    if
		Hook == undefined -> false;
		true -> Hook(is_disabled, {var(Key, I+Index),I+Index,Sto})
	    end;
       (is_minimized, {_,I,Sto}) ->
	    %% This button always has the same minimized state
	    %% as the associated text field.
	    if
		Hook == undefined -> false;
		true -> Hook(is_minimized, {var(Key, I+Index),I+Index,Sto})
	    end;
       (_, _) -> void
    end.

help_hook(Title, HelpLines0) ->
    {hook,
     fun (update, _) ->
	     HelpLines = if is_function(HelpLines0, 0) ->
				 HelpLines0();
			    is_list(HelpLines0) ->
				 HelpLines0 end,
	     wings_help:help_window(Title, HelpLines),
	     keep;
	 (_, _) -> void
     end}.



%%%
%%% Color box.
%%%

-define(COL_PREVIEW_SZ, 60).

-record(col, {}).

mktree_color(RGB, Sto, I, Flags) ->
    #fi{key=Key} = Fi = 
	mktree_leaf(fun col_event/3, enabled, undefined,
		    3*?CHAR_WIDTH,?LINE_HEIGHT+2, I, Flags),
    mktree_priv(Fi, gb_trees:enter(var(Key, I), RGB, Sto), I, #col{}).

col_event({redraw,Active,DisEnabled}, [Fi=#fi{key=Key,index=I}|_], Store) ->
    col_draw(Active, Fi, gb_trees:get(var(Key, I), Store), DisEnabled);
col_event(value, [#fi{key=Key,index=I}|_], Store) ->
    {value,gb_trees:get(var(Key, I), Store)};
col_event({key,_,_,$\s}, [#fi{key=Key,index=I}|_], Store) ->
    pick_color(var(Key, I), Store);
col_event(#mousemotion{x=Xm,y=Ym}, [Fi=#fi{key=Key,index=I}|_], Store) ->
    case col_inside(Xm, Ym, Fi) of
	true -> keep;
	false -> 
	    RGB = gb_trees:get(var(Key, I), Store),
	    {drag,{3*?CHAR_WIDTH,?CHAR_HEIGHT},{color,RGB}}
    end;
col_event(#mousebutton{x=Xm,y=Ym,state=?SDL_RELEASED,button=1}, 
	  [Fi=#fi{key=Key,index=I}|_], Store) ->
    case col_inside(Xm, Ym, Fi) of
	true -> pick_color(var(Key, I), Store);
	false -> keep
    end;
col_event({drop,{color,RGB1}}, [#fi{key=Key,index=I}|_], Store) ->
    K = var(Key, I),
    RGB0 = gb_trees:get(K, Store),
    RGB = replace_rgb(RGB0, RGB1),
    {store,gb_trees:update(K, RGB, Store)};
col_event(_Ev, _Path, _Store) -> keep.

%% replace_rgb(OldRGBA, NewRGBA) -> RGBA
%%  Replace a color (RGB + possibly A) with a new color,
%%  making sure that the new color has the same number of components.
replace_rgb({_,_,_}, {_,_,_}=RGB) -> RGB;
replace_rgb({_,_,_}, {R,G,B,_}) -> {R,G,B};
replace_rgb({_,_,_,_}, {_,_,_,_}=RGB) -> RGB;
replace_rgb({_,_,_,A}, {R,G,B}) -> {R,G,B,A}.

col_draw(Active, #fi{x=X,y=Y0}, RGB, DisEnabled) ->
    FgColor = case DisEnabled of
		  enabled -> color3_text();
		  _ -> color3_disabled()
	      end,
    case DisEnabled of
	enabled ->
	    wings_io:sunken_rect(X, Y0+3, round(1.5*?CHAR_HEIGHT), ?CHAR_HEIGHT,
				 RGB, color4(), Active);
	_ ->
	    wings_io:border(X+1, Y0+3, round(1.5*?CHAR_HEIGHT), ?CHAR_HEIGHT,
			    RGB, FgColor)
    end,
    gl:color3b(0, 0, 0),
    keep.

col_inside(Xm, Ym, #fi{x=X,y=Y}) ->
    inside(Xm, Ym, X, Y, round(1.5*?CHAR_HEIGHT), ?CHAR_HEIGHT+2).

pick_color(Key, Store) ->
    RGB0 = gb_trees:get(Key, Store),
    wings_color:choose(RGB0, fun(RGB) -> {drop,{color,RGB}} end).



%%%
%%% Custom
%%%

-record(custom, {handler}).

mktree_custom(W, H, Handler, Sto, I, Flags) ->
    Fi = mktree_leaf(fun custom_event/3, inert, undefined, W, H, I, Flags),
    mktree_priv(Fi, Sto, I, #custom{handler=Handler}).

custom_event({redraw,_Active,_DisEnabled}, 
	     [#fi{x=X,y=Y,w=W,h=H,index=I}|_], Store) ->
    #custom{handler=Handler} = gb_trees:get(-I, Store),
    Handler(X, Y, W, H, Store);
custom_event(_Ev, _Path, _Store) -> keep.



%%%
%%% Panel
%%%

-record(panel, {}).

mktree_panel(Sto, I, Flags) ->
    Fi = mktree_leaf(fun (_Ev, _Path, _Store) -> keep end, inert, undefined,
		     0, ?LINE_HEIGHT+2, I, Flags),
    mktree_priv(Fi, Sto, I, #panel{}).

%%%
%%% Value
%%%

-record(value, {}).

mktree_value(Value, Sto, I, Flags) ->
    Fi = #fi{key=Key} = 
	mktree_leaf(fun value_event/3, disabled, undefined, 0, 0, I, [no_focus|Flags]),
    mktree_priv(Fi, gb_trees:enter(var(Key, I), Value, Sto), I, #value{}).

value_event(value, [#fi{key=Key,index=I}|_], Store) ->
    {value,gb_trees:get(var(Key, I), Store)};
value_event(_ev, _Path, _Store) -> keep.
    
%%%
%%% Eyepicker.
%%%

-record(eyepicker, {}).

mktree_eyepicker(Sto0, I, Flags) ->
    Fi = #fi{key=Key} = 
	mktree_leaf(fun eyepicker_event/3, inert, undefined, 0, 0, I, Flags),
    Var = var(Key, I),
    Sto = gb_trees_ensure(Var, undefined, Sto0),
    mktree_priv(Fi, Sto, I, #eyepicker{}).

eyepicker_event({picked_color,Col}, 
		[#fi{key=Key,index=I,hook=Hook,flags=Flags}|_], Store)  ->
    hook(Hook, update, [var(Key, I),I,Col,Store,Flags]);
eyepicker_event(_Ev, _Path, _Store) -> keep.

%%%
%%% Label.
%%%

-record(label,
	{lines					%The lines.
	}).

mktree_label(Text, Sto, I, Flags) ->
    Limit = proplists:get_value(break, Flags, infinite),
    {_,Lines} = wings_text:break_lines([Text], Limit),
    Lbl = #label{lines=Lines},
    Fun = fun label_event/3,
    {W,H} = label_dimensions(Lines, 0, 2),
    Fi = mktree_leaf(Fun, enabled, undefined, W, H, I, [no_focus|Flags]),
    mktree_priv(Fi, Sto, I, Lbl).

label_dimensions([L|Lines], W0, H) ->
    case wings_text:width(L) of
	W when W > W0 -> label_dimensions(Lines, W, H+?LINE_HEIGHT);
	_ -> label_dimensions(Lines, W0, H+?LINE_HEIGHT)
    end;
label_dimensions([], W, H) -> {W,H}.

label_event({redraw,_Active,DisEnabled}, [#fi{x=X,y=Y,index=I}|_], Store) ->
    #label{lines=Lines} = gb_trees:get(-I, Store),
    FgColor = case DisEnabled of
		  enabled -> color3_text();
		  _ -> color3_disabled()
	      end,
    gl:color3fv(FgColor),
    label_draw(Lines, X, Y+?CHAR_HEIGHT);
label_event(_Ev, _Path, _Store) -> keep.

label_draw([L|Lines], X, Y) ->
    %gl:color3fv(color3_text()),
    wings_io:text_at(X, Y, L),
    label_draw(Lines, X, Y+?LINE_HEIGHT),
    gl:color3b(0, 0, 0),
    keep;
label_draw([], _, _) -> keep.

%%%
%%% Table field.
%%%

-define(SCROLLER_WIDTH, 13).

-record(table,
	{head,					%Table header.
	 col_widths,				%Column widths.
	 num_els,				%Number of elements.
	 elh,					%Height of each element.
	 rows,					%Number of rows.
	 first=0,				%First row to show.
	 tmarg,					%Top margin.
	 tpos=none				%Tracker position.
	}).

%% Note: In the Store, we keep {[Sel],[Element]}, where the Sel list is
%% the indices of all selected elements (0 or more), and the Element
%% list is the original list of all elements.

mktree_table([Head|Elements], Sto, I, Flags) ->
    NumCols = tuple_size(Head),
    Rows = proplists:get_value(rows, Flags, 20),
    Elh = proplists:get_value(element_height, Flags, ?LINE_HEIGHT),

    %% Default width is 30 for the first columns, 10 for all others.
    ColWidths0 = setelement(1, erlang:make_tuple(NumCols, 10), 30),
    ColWidths1 = proplists:get_value(col_widths, Flags, ColWidths0),
    CharWidth = ?CHAR_WIDTH,
    ColWidths = [Cw*CharWidth || Cw <- tuple_to_list(ColWidths1)],
    W = sum(ColWidths),
    Fun = fun table_event/3,
    TopMarg = ?CHAR_HEIGHT+3,
    H = Rows * Elh + TopMarg + 6,
    T = #table{head=tuple_to_list(Head),col_widths=ColWidths,
	       num_els=length(Elements),elh=Elh,rows=Rows,tmarg=TopMarg},
    #fi{key=Key} = Fi = mktree_leaf(Fun, enabled, undefined, W, H, I, Flags),
    Val = {[],Elements},
    mktree_priv(Fi, gb_trees:enter(var(Key, I), Val, Sto), I, T).

table_event({redraw,Active,DisEnabled}, [#fi{key=Key,index=I}=Fi|_], Sto) ->
    Sel = gb_trees:get(var(Key, I), Sto),
    table_redraw(Fi, gb_trees:get(-I, Sto), Sel, DisEnabled, Active);
table_event(value, [#fi{key=Key,index=I}|_], Sto) ->
    {value,gb_trees:get(var(Key, I), Sto)};
table_event(Ev, [#fi{key=Key,index=I}=Fi|_], Store) ->
    Tab = gb_trees:get(-I, Store),
    Val = gb_trees:get(var(Key, I), Store),
    table_event_1(Ev, Fi, Tab, Val, Store).

table_event_1({focus,false}, #fi{index=I}, Tab, _Val, Sto) ->
    {store,gb_trees:update(-I, Tab#table{tpos=none}, Sto)};
table_event_1(#mousebutton{button=4,state=?SDL_RELEASED}, Fi, Tab, Val, Sto) ->
    table_scroll(-1, Fi, Tab, Val, Sto);
table_event_1(#mousebutton{button=5,state=?SDL_RELEASED}, Fi, Tab, Val, Sto) ->
    table_scroll(1, Fi, Tab, Val, Sto);
table_event_1(#mousebutton{}=Ev, Fi, Tab, Val, Sto) ->
    case table_is_scroll_ev(Ev, Fi, Tab) of
	false -> table_sel_event(Ev, Fi, Tab, Val, Sto);
	true -> table_sc_event(Ev, Fi, Tab, Sto)
    end;
table_event_1(#mousemotion{}=Ev, Fi, Tab, _Val, Sto) ->
    case table_is_scroll_ev(Ev, Fi, Tab) of
	false -> keep;
	true -> table_sc_event(Ev, Fi, Tab, Sto)
    end;
table_event_1({doubleclick,_,_}, _Fi, _Tab, _Val, _Sto) ->
    wings_wm:later(#keyboard{sym=$\r,mod=0,unicode=$\r}),
    keep;
table_event_1(_, _, _, _, _) -> keep.

table_is_scroll_ev(_Ev, _Fi, #table{num_els=N,rows=Rows})
  when N =< Rows -> false;
table_is_scroll_ev(_Ev, _Fi, #table{tpos=Tpos})
  when Tpos =/= none -> true;
table_is_scroll_ev(#mousebutton{x=X,y=Y}, Fi, Tab) ->
    table_is_scroll_ev_1(X, Y, Fi, Tab);
table_is_scroll_ev(#mousemotion{x=X,y=Y}, Fi, Tab) ->
    table_is_scroll_ev_1(X, Y, Fi, Tab).

table_is_scroll_ev_1(_X, Y, _Fi, #table{tmarg=TopMarg})
  when Y < TopMarg -> false;
table_is_scroll_ev_1(X, _Y, #fi{x=FiX,w=FiW}, _)
    when X < FiX+FiW-?SCROLLER_WIDTH -> false;
table_is_scroll_ev_1(_, _, _, _) -> true.

table_redraw(#fi{x=X,y=Y0,w=W,h=H}=Fi,
	     #table{head=Head,col_widths=ColWidths0,elh=Elh,
		    rows=Rows,first=First,tmarg=TopMarg}=Tab,
	     {Sel0,Els0}, _DisEnabled, Active) ->
    ColWidths = table_extend_last_col(ColWidths0, W),
    Ch = ?CHAR_HEIGHT,
    wings_io:sunken_rect(X, Y0+Ch+2, W, H-Ch-4, {0.9,0.9,0.9}, color4(), Active),
    table_draw_head(Head, ColWidths, X, Y0, Ch, Active, TopMarg),
    Els1 = lists:nthtail(First, Els0),
    Sel = [El-First || El <- Sel0],
    Els = table_mark_sel(Els1, Sel),
    Y = Y0 + TopMarg,
    table_draw_els(Els, ColWidths, Rows, X+2, Y, W, Elh),
    table_draw_scroller(Fi, Tab),
    keep.

table_extend_last_col([_], WidthLeft) ->
    [WidthLeft];
table_extend_last_col([Cw|Cws], WidthLeft) ->
    [Cw|table_extend_last_col(Cws, WidthLeft-Cw)].

table_draw_head([H|T], [Cw|ColWidths], X, Y, Ch, Active, TopMarg) ->
    wings_io:sunken_gradient(X, Y+2, Cw, Ch+1,
			     color3_high(), color4(), Active),
    gl:color3fv(color3_text()),
    wings_io:text_at(X+2, Y+TopMarg-2, H),
    gl:color3f(0, 0, 0),
    table_draw_head(T, ColWidths, X+Cw, Y, Ch, Active, TopMarg);
table_draw_head([], [], _, _, _, _, _) -> ok.

table_draw_els(_, _, 0, _, _, _, _) -> ok;
table_draw_els([], _, _, _, _, _, _) -> ok;
table_draw_els([{sel,El}|Els], Cws, Rows, X, Y, W, Elh) ->
    gl:color3f(0, 0, 0.5),
    gl:recti(X-1, Y+3, X+W-6, Y+Elh+3),
    gl:color3f(1, 1, 1),
    table_draw_row(El, 1, Cws, X, Y, Elh),
    gl:color3f(0, 0, 0),
    table_draw_els(Els, Cws, Rows-1, X, Y+Elh, W, Elh);
table_draw_els([El|Els], Cws, Rows, X, Y, W, Elh) ->
    table_draw_row(El, 1, Cws, X, Y, Elh),
    table_draw_els(Els, Cws, Rows-1, X, Y+Elh, W, Elh).

table_draw_row(Cols, I, [Cw|Cws], X, Y, Elh) ->
    {_,Text} = element(I, Cols),
    wings_io:text_at(X, Y+Elh, Text),
    table_draw_row(Cols, I+1, Cws, X+Cw, Y, Elh);
table_draw_row(_, _, [], _, _, _) -> ok.

table_mark_sel(Els, []) -> Els;
table_mark_sel(Els, Sel) -> table_mark_sel_1(Els, 0, Sel).

table_mark_sel_1([E|Els], I, [I|Sel]) ->
    [{sel,E}|table_mark_sel_1(Els, I+1, Sel)];
table_mark_sel_1([E|Els], I, Sel) ->
    [E|table_mark_sel_1(Els, I+1, Sel)];
table_mark_sel_1([], _, _) -> [].

table_sel_event(#mousebutton{button=1,state=?SDL_PRESSED,y=Y},
		#fi{y=YTop,key=Key,index=I,hook=Hook,flags=Flags},
		#table{tmarg=TopMarg,elh=Elh,first=First,num_els=NumEls},
		{_Sel0,Els}, Sto) ->
    Sel = case (Y-YTop-TopMarg) / Elh of
	      N when 0 =< N, N+First < NumEls->
		  [First+trunc(N)];
	      _Outside -> []			%Clear selection.
	  end,
    hook(Hook, update, [var(Key, I),I,{Sel,Els},Sto,Flags]);
table_sel_event(_, _, _, _, _) -> keep.

table_scroll(Dir, #fi{index=I}, #table{first=First0,num_els=N,rows=Rows}=Tab, _Val, Sto) ->
    Incr = Rows div 4,
    First = case First0+Dir*Incr of
		Neg when Neg < 0 -> 0;
		High when High > N-1 -> N-1;
		F -> F
	    end,
    {store,gb_trees:update(-I, Tab#table{first=First}, Sto)}.

table_draw_scroller(_, #table{num_els=NumEls,rows=Rows}) when NumEls =< Rows ->
    ok;
table_draw_scroller(#fi{x=X0,y=Y0,w=W,h=H0},
		    #table{num_els=N,first=First,rows=Rows,tmarg=TopMarg}) ->
    X1 = X0+W-?SCROLLER_WIDTH,
    Y1 = Y0+TopMarg+1,
    H = H0-TopMarg-4,
    wings_io:border(X1, Y1, ?SCROLLER_WIDTH, H, ?PANE_COLOR),
    X = X1+1.5, Y = Y1 + H*First/N + 0.5,
    X2 = X+?SCROLLER_WIDTH-2, Y2 = Y1 + H*(min(First+Rows, N))/N - 0.5,
    gl:shadeModel(?GL_SMOOTH),
    gl:'begin'(?GL_QUADS),
    gl:color3fv(?PANE_COLOR),
    gl:vertex2f(X2, Y),
    gl:vertex2f(X2, Y2),
    gl:color3f(0.7, 0.7, 0.7),
    gl:vertex2f(X, Y2),
    gl:vertex2f(X, Y),
    gl:'end'(),
    gl:shadeModel(?GL_FLAT),
    gl:color3b(0, 0, 0),
    wings_io:border_only(X, Y, X2-X, Y2-Y),
    ok.

table_sc_event(#mousebutton{button=1,y=Y,state=?SDL_PRESSED}, Fi, Tab, Sto) ->
    table_sc_down(Y, Fi, Tab, Sto);
table_sc_event(#mousebutton{button=1,state=?SDL_RELEASED},
	       #fi{index=I}, Tab, Sto) ->
    {store,gb_trees:update(-I, Tab#table{tpos=none}, Sto)};
table_sc_event(#mousemotion{y=Y}, Fi, #table{tpos=Pos}=Tab, Sto)
  when Pos =/= none ->
    table_sc_drag(Y, Fi, Tab, Sto);
table_sc_event(_, _, _, _) -> keep.

table_sc_down(Y0, #fi{index=I,y=FiY,h=H0}=Fi, 
	      #table{num_els=N,first=First,tmarg=TopMarg,rows=Rows}=Tab,
	      Sto) ->
    Y = Y0 - TopMarg - 1 - FiY,
    H = H0-TopMarg-4,
    Pos = Y / H,
    TLow = First/N,
    THigh = min(First+Rows, N)/N,
    if
	Pos < TLow ->
	    table_page_up(Fi, Tab, Sto);
	Pos < THigh ->
	    %% Tracker hit. Start dragging.
	    {store,gb_trees:update(-I, Tab#table{tpos=Pos-TLow}, Sto)};
	true ->
	    table_page_down(Fi, Tab, Sto)
    end.

table_page_down(#fi{index=I}, #table{first=First0,rows=Rows,num_els=N}=Tab, Sto) ->
    First = case First0 + Rows of
		F when N-Rows < F -> N-Rows;
		F -> F
	    end,
    {store,gb_trees:update(-I, Tab#table{first=First}, Sto)}.

table_page_up(#fi{index=I}, #table{first=First0,rows=Rows}=Tab, Sto) ->
    First = case First0 - Rows of
		F when F < 0 -> 0;
		F -> F
	    end,
    {store,gb_trees:update(-I, Tab#table{first=First}, Sto)}.

table_sc_drag(Y0, #fi{index=I,y=FiY,h=H0},
	      #table{num_els=N,tmarg=TopMarg,rows=Rows,tpos=Tpos}=Tab,
	      Sto) ->
    Y = Y0 - TopMarg - 1 - FiY,
    H = H0-TopMarg-4,
    First = case Y/H - Tpos of
		Pos when Pos < 0 -> 0;
		Pos when Pos+Rows/N < 1 -> trunc(N*Pos);
		_ -> N - Rows
	    end,
    {store,gb_trees:update(-I, Tab#table{first=First}, Sto)}.

%%%
%%% Text and number input fields.
%%%

-record(text,
	{bef,					%Reversed list of characters before cursor.
	 aft,					%List of characters after cursor.
	 first=0,			        %First character shown.
	 sel=0,					%Number of characters selected
						%(< 0: leftwards, > 0: righwards)
	 cpos=0,				%Caret pos (pixels).
	 integer=false,				%Integer or not.
	 charset,				%Character set validator.
	 last_val,
	 validator,
         password=false,
	 margin					%Left margin in pixels.
	}).

mktree_text(Val, Sto, I, Flags) ->
    IsInteger = is_integer(Val),
    ValStr = text_val_to_str(Val),
    {Max0,Validator,Charset} = validator(Val, Flags),
    Max = case proplists:get_value(width, Flags) of
	      undefined -> Max0;
	      M when is_integer(M), M >= 1 -> M
	  end,
    Password = proplists:get_bool(password, Flags),
    Cw = ?CHAR_WIDTH,
    W = (1+Max)*Cw,
    Ts = #text{last_val=Val,bef=[],aft=ValStr,
	       integer=IsInteger,charset=Charset,
               validator=Validator,password=Password,
	       margin=Cw div 2},
    Fun = fun gen_text_handler/3,
    Fi = #fi{key=Key} = 
	mktree_leaf(Fun, enabled, undefined,
		    W, ?LINE_HEIGHT+2, I, Flags),
    mktree_priv(Fi, gb_trees:enter(var(Key, I), Val, Sto), I, Ts).

text_val_to_str(Val) when is_float(Val) ->
    wings_util:nice_float(Val);
text_val_to_str(Val) when is_integer(Val) ->
    integer_to_list(Val);
text_val_to_str(Val) when is_list(Val) ->
    Val.

%% -> {MaxFieldWidth,FieldValidator,CharsetPredicate}
%%
validator(Val, Flags) when is_integer(Val) ->
    integer_validator(Flags);
validator(Val, Flags) when is_float(Val) ->
    float_validator(Flags);
validator(Val, Flags) when is_list(Val) ->
    string_validator(Flags).

integer_validator(Flags) ->
    case proplists:get_value(range, Flags) of
	undefined -> {8,accept_all_fun(),latin1_charset()};
	{'-infinity',infinity} -> {8,accept_all_fun(),latin1_charset()};
	{Min,infinity} when is_integer(Min) ->
	    {8,integer_range(Min, infinity),latin1_charset()};
	{'-infinity',Max} when is_integer(Max) ->
	    {8,integer_range('-infinity', Max),latin1_charset()};
	{Min,Max,Default} when is_integer(Min), is_integer(Max), is_integer(Default),
	    Min =< Default, Default =< Max ->
	    Digits = trunc(math:log(Max-Min+1)/math:log(10))+2,
	    {Digits,integer_range(Min, Max, Default),latin1_charset()};
	{Min,Max} when is_integer(Min), is_integer(Max), Min =< Max ->
	    Digits = trunc(math:log(Max-Min+1)/math:log(10))+2,
	    {Digits,integer_range(Min, Max),latin1_charset()}
    end.

float_validator(Flags) ->
    case proplists:get_value(range, Flags) of
	undefined -> {12,accept_all_fun(),latin1_charset()};
	{'-infinity',infinity} -> {12,accept_all_fun(),latin1_charset()};
	{Min,infinity} when is_float(Min) ->
	    {12,float_range(Min, infinity),latin1_charset()};
	{'-infinity',Max} when is_float(Max) ->
	    {12,float_range('-infinity', Max),latin1_charset()};
	{Min,Max,Default} when is_float(Min), is_float(Max), is_float(Default),
	    Min =< Default, Default =< Max ->
	    Digits = min(trunc(math:log(Max-Min+1)/math:log(10))+8, 20),
	    {Digits,float_range(Min, Max, Default),latin1_charset()};
	{Min,Max} when is_float(Min), is_float(Max), Min =< Max ->
	    Digits = min(trunc(math:log(Max-Min+1)/math:log(10))+8, 20),
	    {Digits,float_range(Min, Max),latin1_charset()}
    end.

string_validator(Flags) ->
    Charset = case proplists:get_value(charset, Flags, latin1) of
		  latin1 -> latin1_charset();
		  unicode -> unicode_charset()
	      end,
    {30,accept_all_fun(),Charset}.

latin1_charset() ->
    fun(C) -> C < 256 end.

unicode_charset() ->
    fun(C) -> C < 16#1FFFFF end.

integer_range(Min, Max, Default) ->
    fun(Str) ->
	    case eval_integer(Str) of
		error when Min =/= '-infinity' -> 
		    integer_to_list(Default);
		error when Max =/= infinity -> 
		    integer_to_list(Max);
		Int when Min =/= '-infinity', Int < Min -> 
		    integer_to_list(Default);
		Int when Max =/= infinity, Int > Max -> 
		    integer_to_list(Max);
		Int when is_integer(Int) -> ok
	    end
    end.

integer_range(Min, Max) ->
    fun(Str) ->
	    case eval_integer(Str) of
		error when Min =/= '-infinity' -> 
		    integer_to_list(Min);
		error when Max =/= infinity -> 
		    integer_to_list(Max);
		Int when Min =/= '-infinity', Int < Min -> 
		    integer_to_list(Min);
		Int when Max =/= infinity, Int > Max -> 
		    integer_to_list(Max);
		Int when is_integer(Int) -> ok
	    end
    end.

float_range(Min, Max, Default) ->
    fun(Str) ->
	    case eval_float(Str) of
		error when Min =/= '-infinity' -> 
		    wings_util:nice_float(Default);
		error when Max =/= infinity -> 
		    wings_util:nice_float(Max);
		Float when Min =/= '-infinity', Float < Min -> 
		    wings_util:nice_float(Default);
		Float when Max =/= infinity, Float > Max -> 
		    wings_util:nice_float(Max);
		Float when is_float(Float) -> ok
	    end
    end.

float_range(Min, Max) ->
    fun(Str) ->
	    case eval_float(Str) of
		error when Min =/= '-infinity' -> 
		    wings_util:nice_float(Min);
		error when Max =/= infinity -> 
		    wings_util:nice_float(Max);
		Float when Min =/= '-infinity', Float < Min -> 
		    wings_util:nice_float(Min);
		Float when Max =/= infinity, Float > Max -> 
		    wings_util:nice_float(Max);
		Float when is_float(Float) -> ok
	    end
    end.

accept_all_fun() ->
    fun(_) -> ok end.


%% Does eval two time per character not nice... but it's late...
text_get_val(#text{last_val=OldVal}=Ts) when is_integer(OldVal) ->
    eval_integer(get_text(validate_string(Ts)), OldVal);
text_get_val(#text{last_val=OldVal}=Ts) when is_float(OldVal) ->
    eval_float(get_text(validate_string(Ts)), OldVal);
text_get_val(#text{last_val=Val}=Ts) when is_list(Val) ->
    get_text(Ts).

eval_integer(Str) ->
    eval_integer(Str,error).
eval_integer(Str, Default) ->
    case eval(Str) of
	X when is_float(X) -> round(X);
	X when is_integer(X) -> X;
	_ -> Default
    end.
eval_float(Str) ->
    eval_float(Str, error).    
eval_float(Str, Default) ->
    case eval(Str) of
	X when is_float(X) -> X;
	X when is_integer(X) -> float(X);
	_ -> Default
    end.

eval(Str0) ->
    Str = fix_expr(Str0, []),
    try
	{ok,Tokens,_} = erl_scan:string(Str),
	{ok,Forms} = erl_parse:parse_exprs(Tokens),    
	Bindings = erl_eval:new_bindings(),
	Eval = fun(Form0, {_,Bs0}) ->
		       Form = check_form(Form0),
		       {value,Res,Bs} = erl_eval:expr(Form, Bs0),
		       {Res,Bs}
	       end,
	{Res,_} = lists:foldl(Eval, {error,Bindings}, Forms),
	Res
    catch
	error:_ -> error
    end.

check_form({call,_Line,{remote,_Line2,{atom,_Line3,erlang},{atom,_Line4,_}},_As0}) ->
    exit({forbidden, erlang});
check_form({call,_Line,{remote,_Line2,{atom,_Line3,wings},{atom,_Line4,_}},_As0}) ->
    exit({forbidden, wings});
check_form({call,_Line,{remote,_Line2,{atom,_Line3,esdl},{atom,_Line4,_}},_As0}) ->
    exit({forbidden, esdl});
check_form(Call = {call,Line, {atom,Line3, Func},As0}) ->
    case erlang:is_builtin(math,Func,length(As0)) of
	true -> % Fix math funcs
	    {call,Line,{remote,Line,{atom,Line,math},{atom,Line3,Func}},
	     check_form(As0)};
	false ->
	    Call
    end;
check_form(T) when is_tuple(T) ->
    list_to_tuple(check_form_list(tuple_to_list(T)));
check_form(L) when is_list(L) ->
    check_form_list(L);
check_form(AnyOther) ->
    AnyOther.

check_form_list([H|T]) ->
    [check_form(H)|check_form_list(T)];
check_form_list([]) ->
    [].

fix_expr([], Acc)   -> reverse(Acc, ".");
fix_expr([$.],Acc) -> reverse(Acc, ".");
fix_expr([$.|T], [X|_]=Acc) when X >= $0, X =< $9 ->
    fix_expr(T, [$.|Acc]);
fix_expr([$.|T], Acc)  ->
    fix_expr(T, [$.,$0|Acc]);
%% Some math simplifications.
fix_expr("math:" ++ T, Acc) ->
    fix_expr(T, Acc);
fix_expr("pi" ++ T, Acc) ->
    fix_expr(T, reverse("math:pi()", Acc));
%% Some extra functions.
fix_expr("deg2rad" ++ T, Acc) -> 
    fix_expr(T, reverse("(math:pi()/180)*", Acc));
fix_expr("rad2deg" ++ T, Acc) -> 
    fix_expr(T, reverse("(180/math:pi())*", Acc));
fix_expr([H|T],Acc) ->
    fix_expr(T, [H|Acc]).

gen_text_handler({redraw,Active,DisEnabled}, 
		 [Fi=#fi{key=Key,index=I}|_], Store) ->
    Val = gb_trees:get(var(Key, I), Store),
    draw_text(Fi, gb_trees:get(-I, Store), Val, DisEnabled, Active);
gen_text_handler(value, [#fi{key=Key,index=I}|_], Store) ->
    {value,gb_trees:get(var(Key, I),Store)};
gen_text_handler({focus,true}, [#fi{key=Key,index=I}|_], Sto0) ->
    %% Another field may have updated this field (e.g. a Browse button)
    %% while we were out of focus. Therefore, don't trust anything in
    %% #text{} structure (particularily not 'last_val').
    reset_history(),
    Ts0 = gb_trees:get(-I, Sto0),
    K = var(Key, I),
    Val = gb_trees:get(K, Sto0),
    ValStr = text_val_to_str(Val),
    Ts = Ts0#text{first=0,cpos=0,bef=[],sel=length(ValStr),aft=ValStr},
    Sto = gb_trees:update(-I, Ts#text{last_val=Val},Sto0),
    {store,Sto};
gen_text_handler(Ev, [Fi=#fi{key=Key,index=I,hook=Hook,flags=Flags,w=W}|_], Sto0) ->
    #text{last_val=Val0} = Ts0 = gb_trees:get(-I, Sto0),
    K = var(Key, I),
    Ts1 = case gb_trees:get(K, Sto0) of
	      Val0 -> Ts0;
	      Val1 ->
		  ValStr = text_val_to_str(Val1),
		  Ts0#text{sel=0,first=0,cpos=0,bef=[],aft=ValStr}
	  end,
    Ts2 = text_event(Ev, Fi, Ts1),
    Ts = text_adjust_first(Ts2, W),
    case text_get_val(Ts) of
	Val0 ->
	    {store,gb_trees:update(-I, Ts, Sto0)};
	Val ->
	    Sto = gb_trees:update(-I, Ts#text{last_val=Val},Sto0),
	    hook(Hook, update, [K,I,Val,Sto,Flags])
    end.

text_adjust_first(#text{first=First0,bef=Bef}=Text, Width) ->
    {First,Left} = text_adjust_first_1(First0, Bef, Width),
    Text#text{first=First,cpos=Width-Left}.

text_adjust_first_1(First, Bef, Width) ->
    case length(Bef) of
	Len when Len < First -> {Len,Width};
	Len -> text_adjust_first_2(Bef, First, Len, Width)
    end.

text_adjust_first_2(_, First, NewFirst, Width) when NewFirst =< First ->
    {First,Width};
text_adjust_first_2([C|Cs], First, NewFirst, Width0) ->
    case Width0 - wings_text:width([C]) of
	Width when Width =< 0 -> {NewFirst,Width0};
	Width -> text_adjust_first_2(Cs, First, NewFirst-1, Width)
    end;
text_adjust_first_2([], First, _, Width) ->
    {First,Width}.

draw_text(Fi, Text, Val, DisEnabled, false) ->
    draw_text_inactive(Fi, Text, Val, DisEnabled);
draw_text(Fi, Text, _Val, DisEnabled, true) ->
    draw_text_active(Fi, Text, DisEnabled).

draw_text_inactive(#fi{x=X0,y=Y0,w=Width},
		   #text{first=First,password=Password,margin=Margin},
		   Val, DisEnabled) ->
    Str0 = lists:nthtail(First, text_val_to_str(Val)),
    Str = case Password of
	      true -> stars(Str0);
	      false -> Str0
	  end,
    FgColor =
	case DisEnabled of
	    enabled ->
		wings_io:sunken_gradient(X0, Y0+2,
					 Width, ?CHAR_HEIGHT+1,
					 color3_high(), color4(), false),
		color3_text();
	    _ ->
		blend(fun(Col) ->
			      wings_io:sunken_rect(X0, Y0+2,
						   Width,
						   ?CHAR_HEIGHT+1,
						   Col, Col)
		      end),
		color3_disabled()
	end,
    Y = Y0 + ?CHAR_HEIGHT+1,
    X = X0 + Margin,
    gl:color3fv(FgColor),
    text_draw_fitting(Str, X, Y, Width),
    keep.

draw_text_active(#fi{x=X0,y=Y0,w=Width},
		 #text{sel=Sel,first=First,cpos=CaretPos0,
		       bef=Bef0,aft=Aft0,password=Password,
		       margin=Margin}=Text,
		 DisEnabled) ->
    Ch = ?CHAR_HEIGHT+1,

    wings_io:sunken_gradient(X0, Y0+2, Width, Ch,
			     color3_high(), color4(), true),
    Y = Y0 + Ch,
    X = X0 + Margin,
    {Bef1,Aft} = stars(Password, Bef0, Aft0),
    CaretPos = X + CaretPos0,

    %% Draw caret or selection background.
    {Xpos, SelS} = case {DisEnabled,Sel} of
	{enabled,0} ->
	    gl:color3f(1, 0, 0),
	    wings_io:text_at(CaretPos, Y, [caret]),
	    {0,[]};
	{enabled,N} ->
	    %% Draw highlighted text rectangle
	    gl:color3fv(color5()),
	    if
		N > 0 ->
		    SelStr = lists:sublist(Aft, N),
		    SelW = wings_text:width(SelStr),
		    gl:recti(CaretPos-1, Y-Ch+3, min(CaretPos+SelW, X0+Width), Y+2),
		    {CaretPos, SelStr};
		true ->
		    SelStr = lists:sublist(Bef1, -N),
		    SelW = wings_text:width(SelStr),
		    gl:recti(CaretPos, Y-Ch+3, max(CaretPos-SelW, X0)-1, Y+2),
		    {CaretPos-SelW, reverse(SelStr)}

	    end;
	_ ->
	    {0,[]}
    end,

    %% Draw the text itself.
    gl:color3fv(color3_text()),
    Bef = lists:nthtail(First, reverse(Bef1)),
    wings_io:text_at(X, Y, Bef),
    text_draw_fitting(Aft, CaretPos, Y, Width-CaretPos0),

    %% Draw highlighted text
    gl:color3fv(color6()),
    draw_selected_text(SelS, Xpos, X, Y, X0+Width+1),

    gl:color3b(0, 0, 0),
    Text#text{first=First}.

draw_selected_text([C|SelS], Xpos, X, Y, XMax) when Xpos < X ->
    Cw = wings_text:width([C]),
    draw_selected_text(SelS, Xpos+Cw, X, Y, XMax);
draw_selected_text(_, Xpos, _, _, XMax) when Xpos >= XMax ->
    ok;
draw_selected_text([C|SelS], Xpos0, X, Y, XMax) ->
    CL = [C],
    Cw = wings_text:width(CL),
    Xpos = Cw + Xpos0,
    if Xpos > XMax -> ok;
       true ->
         wings_io:text_at(Xpos0, Y, CL),
         draw_selected_text(SelS, Xpos, X, Y, XMax)
    end;
draw_selected_text([], _, _, _, _) -> ok.

text_draw_fitting([C|Cs], X, Y, W0) ->
    CL = [C],
    Cw = wings_text:width(CL),
    case W0 - Cw of
	W when W =< 0 -> ok;
	W ->
	    wings_io:text_at(X, Y, CL),
	    text_draw_fitting(Cs, X+Cw, Y, W)
    end;
text_draw_fitting([], _, _, _) -> ok.
    
stars(false, Bef, Aft) ->
    {Bef,Aft};
stars(true, Bef, Aft) ->
    {stars(Bef),stars(Aft)}.

stars(N) when is_integer(N) ->
    duplicate(N, $*);
stars(Str) ->
    stars(length(Str)).

validate_string(#text{validator=Validator,sel=Sel0}=Ts) ->
    case Validator(get_text(Ts)) of
	ok -> Ts;
	Str when is_list(Str) ->
	    Sel = case Sel0 of
		      0 -> Sel0;
		      _ -> length(Str)
		  end,
	    Ts#text{bef=[],aft=Str,sel=Sel}
    end.

get_text(#text{bef=Bef,aft=Aft}) ->
    reverse(Bef, Aft).

get_text_r(#text{bef=Bef,aft=Aft}) ->
    reverse(Aft, Bef).

text_event({key,?SDLK_PAGEUP,_,_}, _Fi, Ts=#text{integer=true}) ->
    increment(Ts, 1);
text_event({key,?SDLK_PAGEDOWN,_,_}, _Fi, Ts=#text{integer=true}) ->
    increment(Ts, -1);
text_event({key,_,Mod,Unicode}, _Fi, Ts) ->
    key(Unicode ,Mod, Ts);
text_event({focus,false}, _Fi, Ts0) ->
    add_history(type(Ts0#text.last_val), get_text(Ts0)),
    Ts = validate_string(Ts0),
    Str = get_text(Ts),
    Ts#text{first=0,cpos=0,bef=[],aft=Str,sel=0};
text_event(#mousebutton{x=X,state=?SDL_PRESSED,button=1}, Fi, Ts0) ->
    text_pos(X, Fi, Ts0);
text_event(#mousebutton{x=X,state=?SDL_RELEASED,button=1}, Fi,
	   #text{sel=Sel0,aft=Aft0}=Ts0) ->
    #text{aft=Aft} = Ts = text_pos(X, Fi, Ts0),
    Ts#text{sel=Sel0+length(Aft)-length(Aft0)};
text_event(#mousemotion{x=X}, Fi, Ts) ->
    text_event(#mousebutton{x=X,state=?SDL_RELEASED,button=1}, Fi, Ts);
text_event(_Ev, _Fi, Ts) -> Ts.

text_pos(Mx0, #fi{x=X}, #text{cpos=CaretPos,margin=Margin}=Ts0) ->
    Ts = Ts0#text{sel=0},
    case Mx0 - X - Margin of
	Mx when Mx < CaretPos ->
	    text_pos_left(Mx, Ts);
	Mx ->
	    text_pos_right(Mx, Ts)
    end.

text_pos_left(Mx, #text{bef=[C|Bef],aft=Aft,cpos=CaretPos}=Ts) ->
    case wings_text:width([C]) of
	Cw when Mx =< CaretPos-Cw ->
	    text_pos_left(Mx, Ts#text{bef=Bef,aft=[C|Aft],cpos=CaretPos-Cw});
	_ -> Ts
    end;
text_pos_left(_, Ts) -> Ts.

text_pos_right(Mx, #text{bef=Bef,aft=[C|Aft],cpos=CaretPos}=Ts) ->
    case wings_text:width([C]) of
	Cw when CaretPos+Cw =< Mx ->
	    text_pos_right(Mx, Ts#text{bef=[C|Bef],aft=Aft,cpos=CaretPos+Cw});
	_ -> Ts
    end;
text_pos_right(_, Ts) -> Ts.

key($\b, _, #text{sel=0,bef=[_|Bef]}=Ts) ->	%Bksp (no selection).
    Ts#text{bef=Bef};
key($\b, _, Ts) ->				%Bksp (selection).
    del_sel(Ts);
key($\^B, Mod, #text{sel=Sel0,bef=[C|Bef],aft=Aft}=Ts) ->
    Sel = if
	      ?IS_SHIFTED(Mod) -> Sel0+1;
	      true -> 0
	  end,
    Ts#text{sel=Sel,bef=Bef,aft=[C|Aft]};
key($\^F, Mod, #text{sel=Sel0,bef=Bef,aft=[C|Aft]}=Ts) ->
    Sel = if
	      ?IS_SHIFTED(Mod) -> Sel0-1;
	      true -> 0
	  end,
    Ts#text{sel=Sel,bef=[C|Bef],aft=Aft};
key($\^A, Mod, #text{bef=Bef}=Ts) ->
    Sel = if
	      ?IS_SHIFTED(Mod) -> length(Bef);
	      true -> 0
	  end,
    Ts#text{sel=Sel,bef=[],aft=get_text(Ts)};
key($\^E, Mod, #text{aft=Aft}=Ts) ->
    Sel = if
	      ?IS_SHIFTED(Mod) -> -length(Aft);
	      true -> 0
	  end,
    Ts#text{sel=Sel,bef=get_text_r(Ts),aft=[]};
key($\^K, _, #text{}=Ts) ->
    Ts#text{aft=[]};
key($\^D, _, #text{sel=0,aft=[_|Aft]}=Ts) ->
    Ts#text{aft=Aft};
key($\^D, _, Ts) ->
    del_sel(Ts);
key($\^P, _, #text{}=Ts) ->
    Txt = read_prev_hist(type(Ts#text.last_val), get_text(Ts)),
    Ts#text{sel=0, bef=[], aft=Txt};
key($\^N, _, #text{}=Ts) ->
    Txt = read_next_hist(type(Ts#text.last_val), get_text(Ts)),
    Ts#text{sel=0, bef=[], aft=Txt};
key(C, _, #text{charset=Charset}=Ts0) when $\s =< C ->
    case Charset(C) of
	true ->
	    #text{bef=Bef} = Ts = del_sel(Ts0),
	    Ts#text{bef=[C|Bef]};
	false -> Ts0
    end;
key(_C, _Mod, Ts) -> Ts.

del_sel(#text{sel=Sel,bef=Bef}=Ts) when Sel < 0 ->
    Ts#text{sel=0,bef=lists:nthtail(-Sel, Bef)};
del_sel(#text{sel=Sel,aft=Aft}=Ts) when Sel > 0 ->
    Ts#text{sel=0,aft=lists:nthtail(Sel, Aft)};
del_sel(Ts) -> Ts.

increment(Ts0, Incr) ->
    Str0 = get_text(Ts0),
    case catch list_to_integer(Str0) of
	{'EXIT',_} -> Ts0;
	N ->
	    Str = integer_to_list(N+Incr),
	    Ts = Ts0#text{bef=reverse(Str),aft=[],sel=-length(Str)},
	    validate_string(Ts)
    end.

%%%
%%% Slider
%%%

-define(SL_LENGTH, 150).
-define(SL_BAR_W, 10).
-define(SL_BAR_H, 10).

-record(sl,
	{min,
	 range,
	 color,
	 h
	}).

mktree_slider(Sto0, I, Flags) ->
    {Min,Max} = proplists:get_value(range, Flags),
    Color = case proplists:get_value(color, Flags) of
		undefined -> undefined;
		true -> true;
		{T,_,_}=C when T==r;T==g;T==b;T==h;T==s;T==v -> C
	    end,
    Fi = #fi{key=Key} = 
	mktree_leaf(fun slider_event/3, enabled, undefined,
		    ?SL_LENGTH+?SL_BAR_W, ?LINE_HEIGHT+2, I, Flags),
    Sto = case proplists:get_value(value, Flags) of
	      undefined -> Sto0;
	      Val -> gb_trees:enter(var(Key, I), Val, Sto0)
	  end,
    Sl = #sl{min=Min,range=Max-Min,color=Color,h=?SL_BAR_H},
    mktree_priv(Fi, Sto, I, Sl).

slider_event({redraw,Active,DisEnabled}, [Fi=#fi{key=Key,index=I}|_], Store) ->
    Sl = gb_trees:get(-I, Store),
    Val = gb_trees:get(var(Key, I), Store),
    slider_redraw(Active, Fi, Sl, Val, Store, DisEnabled);
slider_event(value, [#fi{flags=Flags,key=Key,index=I}|_], Store) ->
    case proplists:get_value(value, Flags) of
	undefined ->
	    none;
	_ ->
	    {value,gb_trees:get(var(Key, I), Store)}
    end;
slider_event(#mousebutton{x=Xb,state=?SDL_RELEASED,button=1}, [Fi|_], Store) ->
    slider_event_move(Xb, Fi, Store);
slider_event(#mousemotion{x=Xb,state=Bst}, [Fi|_], Store) 
  when (Bst band ?SDL_BUTTON_LMASK) =/= 0 ->
    slider_event_move(Xb, Fi, Store);
slider_event({key,?SDLK_PAGEUP,_,_}, [Fi|_], Store) ->
    slider_move(10, Fi, Store);
slider_event({key,?SDLK_PAGEDOWN,_,_}, [Fi|_], Store) ->
    slider_move(-10, Fi, Store);
slider_event({key,_,_,$\^A}, 
	     [#fi{key=Key,hook=Hook,index=I,flags=Flags}|_], Store) ->
    #sl{min=Min} = gb_trees:get(-I, Store),
    slider_update(Hook, Min, Key, I, Store, Flags);
slider_event({key,_,_,$\^E}, 
	     [#fi{key=Key,hook=Hook,index=I,flags=Flags}|_], Store) ->
    #sl{min=Min,range=Range} = gb_trees:get(-I, Store),
    slider_update(Hook, Min+Range, Key, I, Store, Flags);
slider_event({key,_,_,$\^F}, [Fi|_], Store) ->
    slider_move(1, Fi, Store);
slider_event({key,_,_,$\^B}, [Fi|_], Store) ->
    slider_move(-1, Fi, Store);
slider_event(_Ev, _Path, _Store) -> keep.

slider_move(D0, #fi{key=Key,index=I,hook=Hook,flags=Flags}, Store) ->
    #sl{min=Min,range=Range} = gb_trees:get(-I, Store),
    Val0 = gb_trees:get(var(Key, I), Store),
    D = if
	     is_integer(Min), D0 > 0 -> max(round(D0*0.01*Range), 1);
	     is_integer(Min), D0 < 0 -> min(round(D0*0.01*Range), -1);
	     is_float(Min) -> D0*0.01*Range
	 end,
    Val = if
	      is_tuple(Val0) -> slider_color_add(D, Val0);
	      true -> max(Min, min(Min+Range, Val0+D))
	  end,
    slider_update(Hook, Val, Key, I, Store, Flags).

slider_event_move(Xb, #fi{x=X,key=Key,index=I,hook=Hook,flags=Flags}, Store) ->
    #sl{min=Min,range=Range} = gb_trees:get(-I, Store),
    Pos = max(0, min(Xb-X, ?SL_LENGTH)),
    V = Min + Pos*Range/?SL_LENGTH,
    Val0 = gb_trees:get(var(Key, I), Store),
    Val = if
	      is_integer(Min) -> round(V);
	      is_tuple(Val0) -> slider_color_move(V, Val0);
	      true -> V
	  end,
    slider_update(Hook, Val, Key, I, Store, Flags).

slider_color_move(V, {R,G,B}) ->
    {H,S,_} = rgb_to_hsv(R, G, B),
    hsv_to_rgb(H, S, V).

slider_color_add(VOffs, {R,G,B}) ->
    {H,S,V0} = rgb_to_hsv(R, G, B),
    V = case V0+VOffs of
	    V1 when V1 < 0.001 -> 0.001;
	    V1 when V1 > 1.0 -> 1.0;
	    V1 -> V1
	end,
    hsv_to_rgb(H, S, V).

slider_update(Hook, Val, Key, I, Store, Flags) ->
    K = var(Key, I),
    hook(Hook, update, [K,I,Val,Store,Flags]).

slider_redraw(Active, Fi, #sl{color=true}=Sl, Val, _Store, DisEnabled) ->
    slider_redraw_1(Active, Fi, Sl, Val, DisEnabled);
slider_redraw(Active, Fi, #sl{color={T,K1,K2}}=Sl, Val, Store, DisEnabled) ->
    V1 = gb_trees:get(K1, Store),
    V2 = gb_trees:get(K2, Store),
    slider_redraw_1(Active, Fi, Sl, {T, {Val,V1,V2}}, DisEnabled);
slider_redraw(Active, Fi, #sl{color=undefined}=Sl, Val, _Store, DisEnabled) ->
    slider_redraw_1(Active, Fi, Sl, Val, DisEnabled).

slider_redraw_1(Active, #fi{x=X,y=Y0,w=W}, #sl{min=Min,range=Range,h=H}, C,
		DisEnabled) ->
    Y = Y0+?LINE_HEIGHT div 2 + 2,
    blend(fun(Col) ->
		  wings_io:gradient_border(X, Y-(H div 2), W, H,
					   Col, {0,0,0}, Active)
	  end),
    Pos = color_slider(C, Min, Range, X, W, Y-(H div 2), H),
    XPos = X+Pos,
    YPos = Y-(?SL_BAR_H div 2)+1,
    case DisEnabled of
	enabled ->
	    Col = color4(),
	    wings_io:raised_rect(XPos+1, YPos, ?SL_BAR_W-1, ?SL_BAR_H-2,
				 Col, Col);
	_ ->
	    blend(fun(Col) ->
			  wings_io:border(XPos, YPos, ?SL_BAR_W, ?SL_BAR_H-2,
					  Col, color3_disabled())
		  end)
    end,
    gl:color3b(0, 0, 0),
    keep.

color_slider({h,{Hue,S,V}}, Min, Range, X, W, Y, H) ->
    gl:shadeModel(?GL_SMOOTH),
    gl:'begin'(?GL_QUADS),
    hue_color_slider(S, V, X, W, Y, H),
    gl:'end'(),
    slider_pos(Hue, Min, Range);
color_slider({R,G,B}, Min, Range, X, W, Y, H) ->
    {Hue,S,V} = rgb_to_hsv(R, G, B),
    SCol = hsv_to_rgb(Hue, S, 0.0),
    ECol = hsv_to_rgb(Hue, S, 1.0),
    gl:shadeModel(?GL_SMOOTH),
    gl:'begin'(?GL_QUADS),
    wings_io:set_color(SCol),
    gl:vertex2f(X+1,Y+H),
    gl:vertex2f(X+1,Y+1),
    wings_io:set_color(ECol),
    gl:vertex2f(X+W,Y+1),
    gl:vertex2f(X+W,Y+H),
    gl:'end'(),
    slider_pos(V, Min, Range);
color_slider(Val, Min, Range, X, W, Y, H) when is_number(Val) ->
    Pos0 = slider_pos(Val, Min, Range),
    Pos = Pos0 + ?SL_BAR_W div 2,
    {R,G,B,A} = Col = color4(),
    Darker = {R-0.15,G-0.15,B-0.15,A},
    wings_io:gradient_rect(X+1, Y+1, Pos, H-1, Darker),
    wings_io:gradient_rect(X+Pos, Y+1, W-Pos, H-1, Col),
    Pos0;
color_slider(C, Min, Range, X, W, Y, H) ->
    {Val,[SCol,ECol]} = get_col_range(C),
    gl:shadeModel(?GL_SMOOTH),
    gl:'begin'(?GL_QUADS),
    wings_io:set_color(SCol),
    gl:vertex2f(X+1,Y+H),
    gl:vertex2f(X+1,Y+1),
    wings_io:set_color(ECol),
    gl:vertex2f(X+W,Y+1),
    gl:vertex2f(X+W,Y+H),
    gl:'end'(),
    slider_pos(Val, Min, Range).

slider_pos(Val, Min, Range) ->
    round(?SL_LENGTH * (Val-Min) / Range).

hue_color_slider(S, V, X, W, Y, H) ->
    wings_io:set_color(hsv_to_rgb(0, S, V)),
    hue_color_slider(S, V, X+1, W-1, Y, H, 0).

hue_color_slider(_, _, _, _, _, _, Hue) when Hue > (360-60) ->
    ok;
hue_color_slider(S, V, X, W, Y, H, Hue) ->
    X0 = X+W*Hue/360.0,
    X1 = X+W*(Hue+60)/360.0,
    gl:vertex2f(X0,Y+H),
    gl:vertex2f(X0,Y+1),
    gl:color3fv(hsv_to_rgb(60+Hue, S, V)),
    gl:vertex2f(X1,Y+1),
    gl:vertex2f(X1,Y+H),
    hue_color_slider(S, V, X, W, Y, H, Hue+60).

get_col_range({r, {R,G,B}}) ->
    {R,[{0,G,B},{1,G,B}]};
get_col_range({g, {G,R,B}}) ->
    {G,[{R,0,B},{R,1,B}]};
get_col_range({b, {B,R,G}}) ->
    {B,[{R,G,0},{R,G,1}]};
get_col_range({s, {S,H,V}}) ->
    S0 = hsv_to_rgb(H,0.0,V),
    S1 = hsv_to_rgb(H,1.0,V),
    {S,[S0,S1]};
get_col_range({v, {V,H,S}}) ->
    V0 = hsv_to_rgb(H,S,0.0),
    V1 = hsv_to_rgb(H,S,1.0),
    {V,[V0,V1]}.


%% Common data storage key. Integer key uses relative field index as
%% storage key, non-integer uses itself.
var(0, I) when is_integer(I) -> I;
var(Key, I) when is_integer(Key), is_integer(I) -> I+Key;
var(Key, I) when is_integer(I) -> Key.


gb_trees_ensure(X, V, T) ->
    case gb_trees:is_defined(X, T) of
	true -> T;
	false -> gb_trees:insert(X, V, T)
    end.


inside(Xm, Ym, X, Y, W, H) ->
    X =< Xm andalso Xm < X+W andalso Y =< Ym andalso Ym < Y+H.


dialog_unzip(L) ->
    dialog_unzip(L, [], []).
dialog_unzip([{Lbl,F}|T], AccA, AccB) ->
    dialog_unzip(T, [{label,Lbl}|AccA], [F|AccB]);
dialog_unzip([], AccA, AccB) ->
    {reverse(AccA),reverse(AccB)}.


blend(Draw) ->
    wings_io:blend(color4(), Draw).


color3_text() ->
    wings_pref:get_value(dialog_text).

color3_disabled() ->
    wings_pref:get_value(dialog_disabled).


color4_highlight() ->
    wings_color:mix(?BEVEL_HIGHLIGHT_MIX, {1,1,1}, color4()).

color4_lowlight() ->
    wings_color:mix(?BEVEL_LOWLIGHT_MIX, {0,0,0}, color4()).

color3_high() ->
    color3(wings_color:mix(0.3*?BEVEL_HIGHLIGHT_MIX, {1,1,1}, color4())).

%%% color3_low() ->
%%%     color3(wings_color:mix(0.3*?BEVEL_LOWLIGHT_MIX, {0,0,0}, color4())).

color3() -> color3(color4()).

color3({R,G,B,_}) -> {R,G,B}.

color4() ->
    wings_pref:get_value(dialog_color).

color5() ->
    wings_pref:get_value(menu_hilite).

color6() ->
    wings_pref:get_value(menu_hilited_text).

rgb_to_hsv({R,G,B}) ->
    rgb_to_hsv(R, G, B).

rgb_to_hsv(R,G,B) ->
    {H,S,V} = wings_color:rgb_to_hsv(R,G,B),
    {round(H),S,V}.

hsv_to_rgb({H,S,V}) ->
    hsv_to_rgb(H, S, V).

hsv_to_rgb(H, S, V) ->
    wings_color:hsv_to_rgb(H, S, V).

hook(Hook, is_disabled, [Var,I,Store]) ->
    case Hook of
	undefined -> keep;
	_ when is_function(Hook, 2) ->
	    case Hook(is_disabled, {Var,I,Store}) of
		void -> keep;
		true -> disabled;
		false -> enabled
	    end
    end;
hook(Hook, is_minimized, [Var,I,Store]) ->
    case Hook of
	undefined -> keep;
	_ when is_function(Hook, 2) ->
	    case Hook(is_minimized, {Var,I,Store}) of
		void -> keep;
		true -> true;
		false -> false
	    end
    end;
hook(Hook, update, [Var,I,Val,Sto0,Flags]) ->
    case gb_trees:get(Var, Sto0) of
	Val -> keep;
	_ ->
	    Result = 
		case Hook of
		    undefined -> {store,gb_trees:update(Var, Val, Sto0)};
		    _ when is_function(Hook, 2) ->
			case Hook(update, {Var,I,Val,Sto0}) of
			    void -> {store,gb_trees:update(Var, Val, Sto0)};
			    %% Result -> Result % but more paranoid
			    keep -> keep;
			    {store,_}=R -> R;
			    done -> done;
			    {done,_}=R -> R;
			    {layout,_}=R -> R
			end
		end,
	    Layout = proplists:get_bool(layout, Flags),
	    case Result of 
		{_,Sto} when Layout -> {layout,Sto};
		_ when Layout -> {layout,Sto0};
		_ -> Result
	    end
    end;
hook(Hook, menu_disabled, [Var,I,Store]) ->
    case Hook of
	undefined -> [];
	_ when is_function(Hook, 2) ->
	    case Hook(menu_disabled, {Var,I,Store}) of
		void -> [];
		Disabled when is_list(Disabled) -> Disabled
	    end
    end.


%%%% History functions

type(Val) when is_integer(Val) -> int;
type(Val) when is_float(Val) -> float;
type(Val) when is_list(Val) -> string.

init_history() ->
    ets:new(wings_history, [named_table]),
    ets:insert(wings_history, {{string, next}, 0}),
    ets:insert(wings_history, {{float, next}, 0}),
    ets:insert(wings_history, {{int, next}, 0}).

add_history(_Type, []) ->  %% No empty strings in history..
    true;
add_history(Type, [_|_]=Val) 
  when Type == float; Type == int; Type == string ->
    [{_,Key}] = ets:lookup(wings_history, {Type,next}),
    case ets:lookup(wings_history, {Type,Key-1}) of
	[{_, Val}] -> %% Already the last history entry
	    true;
	_ ->
	    ets:insert(wings_history, {{Type, Key}, Val}),
	    ets:insert(wings_history, {{Type, next}, Key+1}),
	    ets:delete(wings_history, {Type,pos})
    end.

reset_history() ->
    ets:delete(wings_history, {int,pos}),
    ets:delete(wings_history, {float,pos}),
    ets:delete(wings_history, {string,pos}).

read_prev_hist(Type, Default) ->
    read_hist(Type, Default, -1).
read_next_hist(Type, Default) ->
    read_hist(Type, Default, +1).

read_hist(Type, Default, Step) ->
    [{_, Next}] = ets:lookup(wings_history, {Type,next}),
    Curr0 = case ets:lookup(wings_history, {Type,pos}) of
		[] ->  Next;
		[{_,Key}] ->  Key
	    end,
    Curr1 = Curr0 + Step,
    if Curr1 < 0 ->
	    ets:insert(wings_history, {{Type,pos}, 0}),
	    Default;
       Curr1 >= Next ->
	    ets:delete(wings_history,{Type,pos}),
	    Default;
       true ->
	    ets:insert(wings_history, {{Type,pos}, Curr1}),
	    [{_,Val}] = ets:lookup(wings_history, {Type,Curr1}),
	    Val
    end.
