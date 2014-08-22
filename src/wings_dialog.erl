%%
%%  wings_dialog.erl --
%%
%%     This module (re)implements the dialogs using wxWidgets
%%
%%  Copyright (c) 2013 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(wings_dialog).
-define(NEED_ESDL, 1). %% Needs to send mouseevents to camera
-include("wings.hrl").
-include_lib("wings/e3d/e3d_image.hrl").

-export([init/0,
	 info/3,
	 ask/3, ask/4, ask/5,
	 dialog/3, dialog/4,
	 ask_preview/5, dialog_preview/5
	]).

%% Hook callbacks
-export([enable/3,
	 get_value/2, set_value/3,
	 get_widget/2]).

%%-compile(export_all).

-record(in, {key, type, def, wx, validator, data, hook}).
-record(eh, {fs, apply, owner, type, pid}).

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
%%     Action = ok|preview|cancel|done|function(Result)
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

init() ->
    init_history().

%% Display a text window (Info converted to html)
info(Title, Info, Options) ->
    Parent = proplists:get_value(top_frame, Options, get(top_frame)),
    Style  = {style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER},
    Size   = {size, {500, 400}},
    Dialog = wxDialog:new(Parent, ?wxID_ANY, Title,
			  [Style, Size]),
    Panel  = wxHtmlWindow:new(Dialog, []),
    Sizer  = wxBoxSizer:new(?wxVERTICAL),
    Html = text_to_html(Info),
    wxHtmlWindow:appendToPage(Panel, Html),
    wxSizer:add(Sizer, Panel, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxWindow:setSizer(Dialog, Sizer),
    wxDialog:showModal(Dialog),
    wxDialog:destroy(Dialog),
    keep.


ask_preview(Cmd, Bool, Title, Qs, St) ->
    ask(Bool, Title, preview, Qs, preview_fun(Cmd, St)).

dialog_preview(Cmd, Bool, Title, Qs, St) ->
    dialog(Bool, Title, preview, Qs, preview_fun(Cmd, St)).

%% Currently a modal dialog
%%   (orginal wings let camera events trough)
ask(Title, Qs0, Fun) ->
    {PreviewCmd, Qs} = preview_cmd(Qs0),
    dialog(true, Title, PreviewCmd, queries(Qs), Fun).
ask(Ask, Title, Qs0, Fun) ->
    {PreviewCmd, Qs} = preview_cmd(Qs0),
    dialog(Ask, Title, PreviewCmd, queries(Qs), Fun).
ask(Ask, Title, PreviewCmd, Qs, Fun) ->
    dialog(Ask, Title, PreviewCmd, queries(Qs), Fun).

dialog(Title, Qs0, Fun) ->
    {PreviewCmd, Qs} = preview_cmd(Qs0),
    dialog(true, Title, PreviewCmd, Qs, Fun).
dialog(Ask, Title, Qs0, Fun) ->
    {PreviewCmd, Qs} = preview_cmd(Qs0),
    dialog(Ask, Title, PreviewCmd, Qs, Fun).
dialog(Ask, Title, PreviewCmd, Qs0, Fun) when is_list(Qs0) ->
    Qs = {vframe_dialog, Qs0, [{buttons, [ok, cancel]}]},
    dialog(Ask, Title, PreviewCmd, Qs, Fun);
dialog(Ask, Title, PreviewCmd, Qs, Fun) when not is_list(Qs) ->
    case element(1,Qs) of
	preview -> error(Qs);
	drag_preview_cmd -> error(Qs);
	_Assert -> ok
    end,
    {Dialog, Fields} = build_dialog(Ask andalso PreviewCmd, Title, Qs),
    %% io:format("Enter Dialog ~p ~p ~p~n",[Ask,PreviewCmd, Fields]),
    enter_dialog(Ask, PreviewCmd, Dialog, Fields, Fun).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Hook functions
enable(Keys = [_|_], Bool, Store) ->
    [enable(Key, Bool, Store) || Key <- Keys];
enable(Key, Bool, Store) ->
    #in{wx=Ctrl} = gb_trees:get(Key, Store),
    wxWindow:enable(Ctrl, [{enable,Bool}]).

get_widget(Key, Store) ->
    #in{wx=Wx} = gb_trees:get(Key, Store),
    Wx.

get_value(Key, Store) ->
    In = gb_trees:get(Key, Store),
    case get_output(dummy, In) of
	{_, Value} -> Value;
	Value -> Value
    end.

set_value(Key, Value, Store) ->
    set_value_impl(gb_trees:get(Key, Store),Value),
    Store.

set_value_impl(#in{wx=Ctrl, type=choice}, {Def, Entries}) when is_list(Entries) ->
    wxChoice:clear(Ctrl),
    lists:foldl(fun(Choice,N) -> setup_choices(Choice, Ctrl, Def, N) end, 0, Entries);
set_value_impl(#in{wx=Ctrl, type=choice}, Def) ->
    Count = wxChoice:getCount(Ctrl),
    SetDef = fun(N) ->
		     case wxChoice:getClientData(Ctrl, N) of
			 Def -> wxChoice:setSelection(Ctrl, N), true;
			 _ -> false
		     end
	     end,
    true = lists:any(SetDef, lists:seq(0, Count-1));
set_value_impl(#in{wx=Ctrl, type=text}, Val) ->
    wxTextCtrl:changeValue(Ctrl, to_str(Val));
set_value_impl(#in{wx=Ctrl, type=slider, data={_, ToSlider}}, Val) ->
    wxSlider:setValue(Ctrl, ToSlider(Val)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers

preview_cmd(Preview = {preview, _}) -> Preview;
preview_cmd(Preview = {drag_preview, _}) -> Preview;
preview_cmd(Qs) -> {no_preview, Qs}.

command_name({Menu,Cmd}, Res) ->
    {Menu,{Cmd,Res}};
command_name({Menu,SubMenu,Cmd}, Res) ->
    {Menu,{SubMenu,{Cmd,Res}}}.

preview_fun(Cmd, St) ->
    fun({dialog_preview,Res}) ->
	    Command = command_name(Cmd, Res),
	    {preview,Command,St};
       (cancel) ->
	    St;
       (Res) ->
	    Command = command_name(Cmd, Res),
	    {commit,Command,St}
    end.

queries(Qs0) ->
    {Labels,Vals} = ask_unzip(Qs0),
    [{hframe,
      [{vframe,Labels},
       {vframe,Vals}]}].

enter_dialog(false, _, _, Fields, Fun) -> % No dialog return def values
    Values = [with_key(Field, Def) ||
		 Field = #in{data=Data, def=Def} <- Fields,
		 Data =/= ignore],
    return_result(Fun, Values, wings_wm:this());
enter_dialog(true, no_preview, Dialog, Fields, Fun) -> %% No preview cmd / modal dialog
    case wxDialog:showModal(Dialog) of
	?wxID_CANCEL ->
	    wxDialog:destroy(Dialog),
	    keep;
	Result ->
	    Values = [get_output(Result, Field) ||
			 Field = #in{data=Data} <- Fields,
			 Data =/= ignore],
	    wxDialog:destroy(Dialog),
	    return_result(Fun, Values, wings_wm:this())
    end;
enter_dialog(true, PreviewType, Dialog, Fields, Fun) ->
    Env = wx:get_env(),
    Pid = spawn_link(fun() ->
			     wx:set_env(Env),
			     Forward = fun(Event, _) ->
					       wxDialog:show(Dialog, [{show,false}]),
					       wings_wm:psend(dialog_blanket, Event)
				       end,

			     wxDialog:connect(Dialog, command_button_clicked,
					      [{id, ?wxID_OK},
					       {lastId, ?wxID_NO},
					       {callback,Forward}]),
			     wxDialog:show(Dialog),
			     wings_wm:psend(send_after_redraw, dialog_blanket, preview),
			     receive
				 closed -> wxDialog:destroy(Dialog)
			     end
		     end),
    State = #eh{fs=Fields, apply=Fun, owner=wings_wm:this(),
		type=PreviewType, pid=Pid},
    Op = {push,fun(Ev) -> event_handler(Ev, State) end},
    {TopW,TopH} = wings_wm:top_size(),
    wings_wm:new(dialog_blanket, {0,0,highest}, {TopW,TopH}, Op),
    keep.

notify_event_handler(false, _Msg) -> fun() -> ignore end;
notify_event_handler(no_preview, _) -> fun() -> ignore end;
notify_event_handler(_, Msg) -> fun() -> wings_wm:psend(send_once, dialog_blanket, Msg) end.

event_handler(#wx{id=?wxID_CANCEL},
	      #eh{apply=Fun, owner=Owner, type=Preview, pid=Pid}) ->
    case Preview of
	preview ->
	    #st{}=St = Fun(cancel),
	    wings_wm:send(Owner, {update_state,St});
	drag_preview ->
	    wings_io:grab()
    end,
    Pid ! closed,
    delete;
event_handler(#wx{id=Result}=_Ev,
	      #eh{fs=Fields, apply=Fun, owner=Owner, pid=Pid}) ->
    %%io:format("Ev closing ~p~n  ~p~n",[_Ev, Fields]),
    Values = [get_output(Result, Field) ||
		 Field = #in{data=Data} <- Fields,
		 Data =/= ignore],
    Pid ! closed,
    return_result(Fun, Values, Owner),
    delete;
event_handler(preview, #eh{fs=Fields, apply=Fun, owner=Owner}) ->
    Values = [get_output(preview, Field) ||
		 Field = #in{data=Data} <- Fields,
		 Data =/= ignore],
    case Fun({dialog_preview,Values}) of
	{preview,#st{}=St0,#st{}=St} ->
	    wings_wm:send_after_redraw(Owner, {update_state,St}),
	    wings_wm:send(Owner, {current_state,St0});
	{preview,Action,#st{}=St}->
	    wings_wm:send_once_after_redraw(Owner, {action,Action}),
	    wings_wm:send(Owner, {current_state,St}),
	    keep;
	Action = {numeric_preview, _} ->
	    wings_wm:send(Owner, {action,Action});
	Action when is_tuple(Action); is_atom(Action) ->
	    %%io:format("~p:~p: ~p~n",[?MODULE,?LINE,{preview,[Owner,{action,Action}]}]),
	    wings_wm:send(Owner, {action,Action})
    end;
event_handler(#mousebutton{x=X0,y=Y0}=Ev, _) ->
    {X,Y} = wings_wm:local2global(X0, Y0),
    Win = wings_wm:geom_below(X, Y),
    wings_wm:send(Win, {camera,Ev,keep});
event_handler(#mousemotion{}, _) -> keep;
event_handler(_Ev, _) ->
    %% io:format("unhandled Ev ~p~n",[_Ev]),
    keep.


get_output(Result, In) ->
    get_output1(Result, In).

get_output1(_, In=#in{type=checkbox, wx=Ctrl}) ->
    with_key(In,wxCheckBox:getValue(Ctrl));
get_output1(_, In=#in{type=radiobox, wx=Ctrl, data=Keys}) ->
    ZeroIndex = wxRadioBox:getSelection(Ctrl),
    with_key(In, lists:nth(ZeroIndex+1, Keys));
get_output1(_, In=#in{type=filepicker, wx=Ctrl}) ->
    with_key(In,wxFilePickerCtrl:getPath(Ctrl));
get_output1(_, In=#in{type=color, wx=Ctrl}) ->
    with_key(In, ww_color_ctrl:getColor(Ctrl));
get_output1(_, In=#in{type=slider, wx=Ctrl, data={Convert,_}}) ->
    with_key(In,Convert(wxSlider:getValue(Ctrl)));
%% get_output1(_, In=#in{type=col_slider, wx=Ctrl}) ->
%%     with_key(In,ww_color_slider:getColor(Ctrl));
get_output1(_, In=#in{type=choice, wx=Ctrl}) ->
    with_key(In,wxChoice:getClientData(Ctrl,wxChoice:getSelection(Ctrl)));
get_output1(_, In=#in{type=text, def=Def, wx=Ctrl, validator=Validate}) ->
    Str = wxTextCtrl:getValue(Ctrl),
    Res = validate(Validate, Str, Def),
    with_key(In, Res);
get_output1(Result, In=#in{type=dialog_buttons}) ->
    Atom = case Result of
	       ?wxID_OK -> ok;
	       ?wxID_CANCEL -> cancel;
	       ?wxID_YES -> yes;
	       ?wxID_NO -> no
	   end,
    with_key(In, Atom).

with_key(#in{key=undefined}, Value) -> Value;
with_key(#in{key=Key}, Value) ->  {Key, Value}.

setup_hooks(Fields) ->
    {Fs, _} = lists:mapfoldl(fun(In=#in{key=undefined},N) -> {{N, In}, N+1};
				(In=#in{key=Key}, N) -> {{Key, In}, N+1}
			     end, 1,Fields),
    Tree = gb_trees:from_orddict(lists:sort(Fs)),
    [setup_hook(Field, Tree) || Field <- Fs].

setup_hook({_, #in{hook=undefined}}, _) -> ok;
setup_hook({Key, #in{wx=Ctrl, type=checkbox, hook=UserHook}}, Fields) ->
    %% Setup callback
    wxWindow:connect(Ctrl, command_checkbox_clicked,
		     [{callback, fun(#wx{event=#wxCommand{commandInt=Int}}, Obj) ->
					 wxEvent:skip(Obj),
					 UserHook(Key, Int =/= 0, Fields)
				 end}]),
    %% And initiate
    UserHook(Key, wxCheckBox:getValue(Ctrl), Fields);
setup_hook({Key, #in{wx=Ctrl, type=choice, hook=UserHook}}, Fields) ->
    wxWindow:connect(Ctrl, command_choice_selected,
		     [{callback, fun(#wx{event=#wxCommand{commandInt=Int}}, Obj) ->
					 wxEvent:skip(Obj),
					 Sel = wxChoice:getClientData(Ctrl, Int),
					 UserHook(Key, Sel, Fields)
				 end}]),
    UserHook(Key, wxChoice:getClientData(Ctrl,wxChoice:getSelection(Ctrl)), Fields);
setup_hook({Key, #in{wx=Ctrl, type=radiobox, hook=UserHook, data=Keys}}, Fields) ->
    wxWindow:connect(Ctrl, command_radiobox_selected,
		     [{callback, fun(#wx{event=#wxCommand{commandInt=ZeroIndex}}, Obj) ->
					 wxEvent:skip(Obj),
					 Sel = lists:nth(ZeroIndex+1, Keys),
					 UserHook(Key, Sel, Fields)
				 end}]),
    UserHook(Key, lists:nth(1+wxRadioBox:getSelection(Ctrl),Keys),Fields);
setup_hook({Key, #in{wx=Ctrl, type=text, hook=UserHook, def=Def, validator=Validate}}, Fields) ->
    wxWindow:connect(Ctrl, command_text_updated,
		     [{callback, fun(#wx{event=#wxCommand{cmdString=Str}}, Obj) ->
					 wxEvent:skip(Obj),
					 Val = validate(Validate, Str, Def),
					 UserHook(Key, Val, Fields),
					 ok
				 end}]),
    UserHook(Key,validate(Validate, wxTextCtrl:getValue(Ctrl), Def),Fields);
setup_hook({Key, #in{wx=Ctrl, type=color, hook=UserHook}}, Fields) ->
    ww_color_ctrl:connect(Ctrl, col_changed,
			  [{callback, fun({col_changed, Col}) ->
					      UserHook(Key, Col, Fields)
				      end}]),
    ok;

%% Kind of special
setup_hook({_Key, #in{wx=Canvas, type=custom_gl, hook={paint, CustomRedraw}}}, Fields) ->
    Env = wx:get_env(),
    Custom = fun() ->
		     wxGLCanvas:setCurrent(Canvas),
		     CustomRedraw(Canvas, Fields),
		     wxGLCanvas:swapBuffers(Canvas)
	     end,
    Redraw = fun(#wx{}, _) ->
		     case os:type() of
			 {win32, _} ->
			     DC = wxPaintDC:new(Canvas),
			     wxPaintDC:destroy(DC);
			 _ -> ok
		     end,
		     spawn(fun() ->
				   wx:set_env(Env),
				   wx:batch(Custom)
			   end),
		     ok
	     end,
    wxWindow:connect(Canvas, paint, [{callback, Redraw}]),
    wxWindow:connect(Canvas, erase_background, [{callback, fun(_,_) -> ok end}]), %% WIN32 only?
    ok;

setup_hook(_What, _) ->
    io:format("Unknown hook for ~p~n",[_What]),
    ok.

return_result(Fun, Values, Owner) ->
    case Fun(Values) of
	ignore ->
	    ok;
	#st{}=St ->
	    wings_wm:send(Owner, {new_state,St});
	{commit,#st{}=St0,#st{}=St}->
	    wings_wm:send(Owner, {current_state,St0}),
	    wings_wm:send_after_redraw(Owner, {new_state,St});
	{commit,Action,#st{}=St}->
	    wings_wm:send(Owner, {current_state,St}),
	    wings_wm:send_after_redraw(Owner, {action,Action});
	Action when is_tuple(Action); is_atom(Action) ->
	    wings_wm:send(Owner, {action,Action})
    end,
    keep.

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
    {lists:reverse(Labels),lists:reverse(Vals)}.

build_dialog(false, _Title, Qs) ->
    Fs = build(false, Qs, undefined, undefined, []),
    {undefined, lists:reverse(Fs)};
build_dialog(AskType, Title, Qs) ->
    wx:batch(fun() ->
		     Parent = get(top_frame),
		     Style  = {style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER},
		     Dialog = wxDialog:new(Parent, ?wxID_ANY, Title, [Style]),
		     Panel  = wxPanel:new(Dialog, []),
		     Top    = wxBoxSizer:new(?wxVERTICAL),
		     Sizer  = wxBoxSizer:new(?wxVERTICAL),
		     Fields0 = build(AskType, Qs, Panel, Sizer, []),
		     wxWindow:setSizer(Panel, Sizer),
		     wxSizer:add(Top, Panel, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
		     Fields = setup_buttons(Dialog, Top, Fields0),
		     wxWindow:setSizerAndFit(Dialog, Top),
		     setup_hooks(Fields),
		     {Dialog, Fields}
	     end).

setup_buttons(Dialog, Top, [DB=#in{type=dialog_buttons, wx=Fun}|In]) ->
    Object = Fun(Dialog, Top),
    lists:reverse([DB#in{wx=Object}|In]);
setup_buttons(_, _, Fields) ->
    lists:reverse(Fields).

build(Ask, {vframe_dialog, Qs, Flags}, Parent, Sizer, []) ->
    Def = proplists:get_value(value, Flags, ?wxID_OK),
    Buttons = proplists:get_value(buttons, Flags, [ok, cancel]),
    ButtMask = lists:foldl(fun(ok, Butts)     -> ?wxOK bor Butts;
			      (cancel, Butts) -> ?wxCANCEL bor Butts;
			      (yes, Butts)    -> ?wxYES bor Butts;
			      (no,  Butts)    -> ?wxNO bor Butts
			   end, 0, Buttons),
    Create = fun(Dialog, TopSizer) ->
		     Ok = wxDialog:createButtonSizer(Dialog, ButtMask),
		     wxSizer:add(TopSizer, Ok, [{proportion, 0},
						{flag, ?wxEXPAND bor ?wxALL},
						{border, 5}]),
		     case Ask of
			 no_preview -> %% I.e. non preview
			     Close = fun(#wx{id=Id},_) ->
					     wxDialog:endModal(Dialog, Id)
				     end,
			     wxDialog:connect(Dialog, command_button_clicked,
					      [{id, ?wxID_NO}, {callback,Close}]);
			 _ ->
			     ignore %% Preview connects it self
		     end,
		     Ok
	     end,
    In = build(Ask, {vframe, Qs}, Parent, Sizer, []),
    [#in{key=proplists:get_value(key,Flags), def=Def, data=proplists:get_value(key,Flags,ignore),
	 type=dialog_buttons, wx=Create}|In];

build(Ask, {oframe, Tabs, 1, Flags}, Parent, WinSizer, In0)
  when Ask =/= false ->
    buttons =:= proplists:get_value(style, Flags, buttons) orelse error(Flags),
    NB = wxNotebook:new(Parent, ?wxID_ANY, []),
    AddPage = fun({Title, Data}, In) ->
		      Panel = wxPanel:new(NB, []),
		      case os:type() of
			  {win32,_} ->
			      BG = wxNotebook:getThemeBackgroundColour(NB),
			      wxPanel:setBackgroundColour(Panel, BG);
			  _ -> ignore
		      end,
		      Sizer  = wxBoxSizer:new(?wxVERTICAL),
		      Out = build(Ask, Data, Panel, Sizer, In),
		      wxPanel:setSizerAndFit(Panel, Sizer),
		      wxNotebook:addPage(NB, Panel, Title),
		      Out
	      end,
    In = lists:foldl(AddPage, In0, Tabs),
    wxSizer:add(WinSizer, NB, [{proportion, 1},{flag, ?wxEXPAND}]),
    In;
build(Ask, {vframe, Qs}, Parent, Sizer, In) ->
    build(Ask, {vframe, Qs, []}, Parent, Sizer, In);
build(Ask, {vframe, Qs, Flags}, Parent, Sizer, In) ->
    build_box(Ask, ?wxVERTICAL, Qs, Flags, Parent, Sizer, In);
build(Ask, {hframe, Qs}, Parent, Sizer, In) ->
    build(Ask, {hframe, Qs, []}, Parent, Sizer, In);
build(Ask, {hframe, Qs, Flags}, Parent, Sizer, In) ->
    build_box(Ask, ?wxHORIZONTAL, Qs, Flags, Parent, Sizer, In);

build(Ask, {vradio, Alternatives, Def}, Parent, Sizer, In) ->
    build(Ask, {vradio, Alternatives, Def, []}, Parent, Sizer, In);
build(Ask, {vradio, Alternatives, Def, Flags}, Parent, Sizer, In) ->
    build_radio(Ask, Def, {?wxVERTICAL, ?wxRA_SPECIFY_COLS},
		Alternatives, Flags, Parent, Sizer, In);
build(Ask, {hradio, Alternatives, Def}, Parent, Sizer, In) ->
    build(Ask, {hradio, Alternatives, Def, []}, Parent, Sizer, In);
build(Ask, {hradio, Alternatives, Def, Flags}, Parent, Sizer, In) ->
    build_radio(Ask, Def, {?wxHORIZONTAL, ?wxRA_SPECIFY_ROWS},
		Alternatives, Flags, Parent, Sizer, In);

build(Ask, {label, Label}, Parent, Sizer, In) ->
    build(Ask, {label, Label, []}, Parent, Sizer, In);
build(Ask, {label, Label, Flags}, Parent, Sizer, In)
  when Ask =/= false ->
    Limit = proplists:get_value(break, Flags, infinite),
    {_,Lines0=[First|_]} = wings_text:break_lines([Label], Limit),
    Lines = lists:foldr(fun(Row, Acc) when Row =:= First -> [Row|Acc];
			   (Row, Acc) -> ["\n", Row|Acc]
			end, [], Lines0),
    Text = wxStaticText:new(Parent, ?wxID_ANY, Lines),
    add_sizer(label, Sizer, Text),
    wxSizer:setItemMinSize(Sizer, Text, proplists:get_value(min_wsz, Flags, -1), -1),
    In;
build(Ask, {label_column, Rows}, Parent, Sizer, In) ->
    build(Ask, {label_column, Rows, []}, Parent, Sizer, In);
build(Ask, {label_column, Rows, Flags}, Parent, Sizer, In) ->
    MinSize =
	if Ask =:= false -> -1;
	   true ->
		ST = wxStaticText:new(Parent, ?wxID_ANY, ""),
		lists:foldl(fun({Str = [_|_],_}, Max) ->
				    {W, _, _, _} = wxWindow:getTextExtent(ST, Str),
				    max(W+5, Max);
			       (separator, Max)  ->
				    Max
			    end, -1, Rows)
	end,
    Translate = fun({String, Field}) ->
			{hframe, [{label, String, [{min_wsz, MinSize}]}, Field]};
		   (separator) ->
			separator
		end,
    build(Ask, {vframe, lists:map(Translate, Rows), Flags}, Parent, Sizer, In);

build(Ask, panel, _Parent, Sizer, In)
  when Ask =/= false ->
    wxSizer:addSpacer(Sizer, 20),
    In;
build(Ask, separator, Parent, Sizer, In)
  when Ask =/= false ->
    Separator = wxStaticLine:new(Parent),
    add_sizer(separator, Sizer, Separator),
    In;

build(Ask, {text, Def}, Parent, Sizer, In) ->
    build(Ask, {text, Def, []}, Parent, Sizer, In);
build(Ask, {text, Def, Flags}, Parent, Sizer, In) ->
    {_Max0,Validator} = validator(Def, Flags),
    Create = fun() ->
		     PreviewFun = notify_event_handler(Ask, preview),
		     Ctrl = wxTextCtrl:new(Parent, ?wxID_ANY, [{value, to_str(Def)}]),
		     tooltip(Ctrl, Flags),
		     Type = type(Def),
		     TextUpdated = fun(#wx{event=#wxCommand{cmdString=Str}},_) ->
					   case Validator(Str) of
					       {true, _} -> PreviewFun();
					       false -> ignore
					   end
				   end,
		     UseHistory = fun(Ev, Obj) ->
					  case use_history(Ev, Type, Ctrl) of
					      {true, _Prev}  ->
						  PreviewFun();
					      false ->
						  wxEvent:skip(Obj)
					  end
				  end,
		     AddHistory = fun(_,Obj) ->
					  Str = wxTextCtrl:getValue(Ctrl),
					  wxEvent:skip(Obj),
					  case Validator(Str) of
					      {true, _} -> add_history(Type, Str);
					      false -> ignore
					  end
				  end,
		     wxTextCtrl:connect(Ctrl, key_up, [{callback, UseHistory}]),
		     wxTextCtrl:connect(Ctrl, command_text_updated, [{callback, TextUpdated}]),
		     wxTextCtrl:connect(Ctrl, kill_focus, [{callback, AddHistory}]),
		     add_sizer(text, Sizer, Ctrl),
		     Ctrl
	     end,
    [#in{key=proplists:get_value(key,Flags), def=Def, hook=proplists:get_value(hook, Flags),
	 type=text, wx=create(Ask,Create), validator=Validator}|In];

build(Ask, {slider, {text, Def, Flags}}, Parent, Sizer, In) ->
    {_Max0,Validator} = validator(Def, Flags),
    Create = fun() -> create_slider(Ask, Def, Flags, Validator, Parent, Sizer) end,
    [#in{key=proplists:get_value(key,Flags), def=Def,
	 hook=proplists:get_value(hook, Flags),
	 type=text, wx=create(Ask,Create), validator=Validator}|In];

build(Ask, {slider, {color, Def, Flags}}, Parent, Sizer, In) ->
    Create = fun() ->
		     SS = wxBoxSizer:new(?wxHORIZONTAL),
		     SliderCtrl = ww_color_slider:new(Parent, ?wxID_ANY, Def),
		     ColCtrl = ww_color_ctrl:new(Parent, ?wxID_ANY, [{col, Def}]),

		     wxSizer:add(SS, SliderCtrl, [{proportion,2}, {flag, ?wxEXPAND}]),
		     wxSizer:add(SS, ColCtrl, [{proportion,0}]),
		     tooltip(SliderCtrl, Flags),
		     tooltip(ColCtrl, Flags),
		     add_sizer(slider, Sizer, SS),
		     UpdateSlider =
			 fun({col_changed, Col}) ->
				 ww_color_slider:setColor(SliderCtrl, Col)
			 end,
		     UpdateCtrl =
			 fun({col_changed, Col}) ->
				 ww_color_ctrl:setColor(ColCtrl, Col)
			 end,
		     ok = ww_color_ctrl:connect(ColCtrl, col_user_set,
						[{callback, UpdateSlider}]),
		     ok = ww_color_slider:connect(SliderCtrl, col_changed,
						  [{callback, UpdateCtrl}]),
		     ColCtrl
	     end,
    [#in{key=proplists:get_value(key,Flags), def=Def,
	 hook=proplists:get_value(hook, Flags),
	 type=color, wx=create(Ask,Create)}|In];


build(Ask, {slider, Flags}, Parent, Sizer, In) ->
    Def = proplists:get_value(value, Flags),
    Range = proplists:get_value(range, Flags),
    false = undefined == Def,
    false = undefined == Range,
    {Min, Value, Max, Style, ToText, ToSlider} = slider_style(Def, Range),
    Create = fun() ->
		     Ctrl = wxSlider:new(Parent, ?wxID_ANY, Value, Min, Max, [{style, Style}]),
		     tooltip(Ctrl, Flags),
		     add_sizer(slider, Sizer, Ctrl),
		     Ctrl
	     end,
    [#in{key=proplists:get_value(key,Flags), def=Def, data={ToText, ToSlider},
	 hook=proplists:get_value(hook, Flags),
	 type=slider, wx=create(Ask,Create)}|In];

build(Ask, {color, Def, Flags}, Parent, Sizer, In) ->
    Create = fun() ->
		     Ctrl = ww_color_ctrl:new(Parent, ?wxID_ANY, [{col, Def}]),
		     tooltip(Ctrl, Flags),
		     add_sizer(button, Sizer, Ctrl),
		     Ctrl
	     end,
    [#in{key=proplists:get_value(key,Flags), def=Def,
	 type=color, wx=create(Ask,Create)}|In];

build(Ask, {button, {text, Def, Flags}}, Parent, Sizer, In) ->
    Create = fun() ->
		     Props = proplists:get_value(props, Flags, []),
		     What = case proplists:get_value(dialog_type, Props, open_dialog) of
				open_dialog -> ?wxFLP_OPEN;
				save_dialog -> ?wxFLP_SAVE
			    end,
		     Filter = wings_file:file_filters(Props),
		     Ctrl = wxFilePickerCtrl:new(Parent, ?wxID_ANY,
						 [{style, What bor ?wxFLP_USE_TEXTCTRL},
						  {path, Def},
						  {wildcard, Filter}]),
		     tooltip(Ctrl, Flags),
		     add_sizer(button, Sizer, Ctrl),
		     Ctrl
	     end,
    [#in{key=proplists:get_value(key,Flags), def=Def,
	 type=filepicker, wx=create(Ask,Create)}|In];

build(Ask, {menu, Entries, Def, Flags}, Parent, Sizer, In) ->
    Create =
	fun() ->
		Ctrl = wxChoice:new(Parent, ?wxID_ANY),
		lists:foldl(fun(Choice,N) -> setup_choices(Choice, Ctrl, Def, N) end, 0, Entries),
		tooltip(Ctrl, Flags),
		add_sizer(choice, Sizer, Ctrl),
		Ctrl
	end,
    [#in{key=proplists:get_value(key,Flags), def=Def, hook=proplists:get_value(hook, Flags),
	 type=choice, wx=create(Ask,Create)}|In];

build(Ask, {table, [Header|Rows], Flags}, Parent, Sizer, In) ->
    Create =
	fun() ->
		Options = [{style, ?wxLC_REPORT},
			   {size, {min(tuple_size(Header)*80, 500),
				   min((2+length(Rows))*25, 800)}}],
		Ctrl = wxListCtrl:new(Parent, Options),
		AddHeader = fun(HeadStr, Column) ->
				    wxListCtrl:insertColumn(Ctrl, Column, HeadStr, []),
				    Column + 1
			    end,
		lists:foldl(AddHeader, 0, tuple_to_list(Header)),
		case proplists:get_value(col_widths, Flags) of
		    undefined -> ok;
		    Widths ->
			SetWidth = fun(Width, Column) ->
					   wxListCtrl:setColumnWidth(Ctrl, Column, Width*8),
					   Column + 1
				   end,
			lists:foldl(SetWidth, 0, tuple_to_list(Widths))
		end,
		Add = fun({_, Str}, {Row, Column}) ->
			      wxListCtrl:setItem(Ctrl, Row, Column, Str),
			      {Row, Column+1}
		      end,
		lists:foldl(fun(Row, N) ->
				    wxListCtrl:insertItem(Ctrl, N, ""),
				    lists:foldl(Add, {N, 0}, tuple_to_list(Row)),
				    N + 1
			    end, 0, Rows),
		add_sizer(table, Sizer, Ctrl),
		Ctrl
	end,
    create(Ask,Create),
    %% [#in{key=proplists:get_value(key,Flags), def=Rows,
    %% 	 type=table, wx=create(Ask,Create)}|In];
    In;

build(Ask, {image, ImageOrFile}, Parent, Sizer, In) ->
    Create = fun() ->
		     Bitmap = image_to_bitmap(ImageOrFile),
		     SBMap = wxStaticBitmap:new(Parent, ?wxID_ANY, Bitmap),
		     add_sizer(image, Sizer, SBMap),
		     wxBitmap:destroy(Bitmap),
		     SBMap
	     end,
    create(Ask, Create),
    In;

build(Ask, {help, Title, Fun}, Parent, Sizer, In) ->
    TopFrame = get(top_frame),
    Display = fun(_,_) ->
		      info(Title, Fun(), [{top_frame, TopFrame}])
	      end,
    Create = fun() ->
		     Button = wxButton:new(Parent, ?wxID_HELP),
		     wxButton:connect(Button, command_button_clicked, [{callback, Display}]),
		     add_sizer(button, Sizer, Button),
		     Button
	     end,
    create(Ask,Create),
    In;

build(Ask, {custom_gl, CW, CH, Fun}, Parent, Sizer, In) ->
    build(Ask, {custom_gl, CW, CH, Fun, []}, Parent, Sizer, In);
build(Ask, {custom_gl, CW, CH, Fun, Flags}, Parent, Sizer, In) ->
    Context = wxGLCanvas:getContext(get(gl_canvas)),
    Create = fun() ->
		     Canvas = wxGLCanvas:new(Parent, Context,
					     [{size, {CW,CH}},
					      {attribList, wings_init:gl_attributes()}
					     ]),
		     add_sizer(custom, Sizer, Canvas),
		     Canvas
	     end,
    [#in{key=proplists:get_value(key,Flags),
	 type=custom_gl, data=ignore, hook={paint, Fun}, wx=create(Ask, Create)}|In];

build(Ask, {Label, Def}, Parent, Sizer, In) ->
    build(Ask, {Label, Def, []}, Parent, Sizer, In);
build(Ask, {Label, Def, Flags}, Parent, Sizer, In)
  when is_boolean(Def) ->
    Create = fun() ->
		     Ctrl = wxCheckBox:new(Parent, ?wxID_ANY, Label),
		     tooltip(Ctrl, Flags),
		     wxCheckBox:setValue(Ctrl, Def),
		     add_sizer(checkbox, Sizer, Ctrl),
		     Ctrl
	     end,
    [#in{key=proplists:get_value(key,Flags), hook=proplists:get_value(hook, Flags),
	 def=Def, type=checkbox, wx=create(Ask, Create)}|In];

build(false, _Q, _Parent, _Sizer, In) ->
    In;
build(Ask, Q, _Parent, _Sizer, In) ->
    io:format("~p:~p: Unhandled ask=~p, ~P~n  From: ~p~n",
	      [?MODULE,?LINE,Ask,Q,10, erlang:process_info(self(), current_stacktrace)]),
    In.

build_box(false, _Type, Qs, _, Parent, _Top, In0) ->
    lists:foldl(fun(Q, Input) ->
			build(false, Q, Parent, undefined, Input)
		end, In0, Qs);
build_box(Ask, Type, Qs, Flags, Parent, Top, In0) ->
    Sizer = case proplists:get_value(title, Flags) of
		undefined -> wxBoxSizer:new(Type);
		Title -> wxStaticBoxSizer:new(Type, Parent, [{label, Title}])
	    end,
    Input = lists:foldl(fun(Q, Input) ->
				build(Ask, Q, Parent, Sizer, Input)
			end, In0, Qs),
    add_sizer({box, Type}, Top, Sizer),
    Input.

build_radio(Ask, Def, {Dir, Style}, Alternatives, Flags, Parent, Sizer, In) ->
    Name = proplists:get_value(title, Flags, ""),
    {Strs,Keys} = lists:unzip(Alternatives),
    true = lists:member(Def, Keys),
    Create = fun() ->
		     Ctrl = wxRadioBox:new(Parent, 1, Name,
					   ?wxDefaultPosition, ?wxDefaultSize,
					   Strs, [{majorDim, 1}, {style, Style}]),
		     add_sizer({radiobox, Dir}, Sizer, Ctrl),
		     tooltip(Ctrl, Flags),
		     wxRadioBox:setSelection(Ctrl, pos(Def, Keys)),
		     Preview = fun(_, _) -> (notify_event_handler(Ask, preview))() end,
		     wxRadioBox:connect(Ctrl, command_radiobox_selected,
					[{callback, Preview}]),
		     Ctrl
	     end,
    [#in{key=proplists:get_value(key,Flags), hook=proplists:get_value(hook, Flags),
	 def=Def, type=radiobox, wx=create(Ask, Create),
	 data=Keys}|In].


setup_choices({Str, Tag}, Ctrl, Def, N) ->
    wxChoice:append(Ctrl, Str, Tag),
    Def =:= Tag andalso wxChoice:setSelection(Ctrl, N),
    N + 1;
setup_choices({Str, Tag, Fs}, Ctrl, Def, N) ->
    TT = proplists:get_value(info, Fs, ""),
    wxWindow:setToolTip(Ctrl, wxToolTip:new(TT)),
    wxChoice:append(Ctrl, Str, Tag),
    Def =:= Tag andalso wxChoice:setSelection(Ctrl, N),
    N + 1.


create_slider(Ask, Def, Flags, Validator, Parent, TopSizer) when is_number(Def) ->
    Range = proplists:get_value(range, Flags),
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    {Min, Value, Max, Style, ToText, ToSlider} = slider_style(Def, Range),
    Slider = wxSlider:new(Parent, ?wxID_ANY, Value, Min, Max, [{style, Style}]),
    Text = wxTextCtrl:new(Parent, ?wxID_ANY, [{value, to_str(Def)}]),
    wxSizer:add(Sizer, Slider, [{proportion,2}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, Text,   [{proportion,1}]),
    add_sizer(slider, TopSizer, Sizer),
    tooltip(Slider, Flags),
    tooltip(Text, Flags),
    case proplists:get_value(disable, Flags, false) of
	true -> wxWindow:disable(Slider),wxWindow:disable(Text);
	false -> ok
    end,

    PreviewFun = notify_event_handler(Ask, preview),
    UpdateText = fun(#wx{event=#wxCommand{commandInt=Where}}, _) ->
			 PreviewFun(),
			 wxTextCtrl:setValue(Text, to_str(ToText(Where)))
		 end,
    wxSlider:connect(Slider, command_slider_updated, [{callback, UpdateText}]),
    UpdateSlider = fun(#wx{event=#wxCommand{cmdString=Str}}, _) ->
			   case Validator(Str) of
			       {true,Float} ->
				   PreviewFun(),
				   wxSlider:setValue(Slider, ToSlider(Float));
			       _ ->
				   ignore
			   end
		   end,
    wxTextCtrl:connect(Text, command_text_updated, [{callback, UpdateSlider}]),
    Text.

slider_style(Def, {Min, Max})
  when is_integer(Def), Def >= Min, Def =< Max, Min < Max ->
    ToInt = fun(Value) -> Value end,
    {Min, Def, Max, ?wxSL_HORIZONTAL, ToInt, ToInt};
slider_style(Def, {Min, Max})
  when is_float(Def), Def >= Min, Def =< Max, Min < Max ->
    ToSlider = fun(Value) ->
		       Step = (Max - Min) / 100,
		       round((Value - Min) / Step)
	       end,
    ToText = fun(Percent) ->
		     Step = (Max - Min) / 100,
		     Min + Percent * Step
	     end,
    {0, ToSlider(Def), 100, ?wxSL_HORIZONTAL, ToText, ToSlider}.

add_sizer(What, Sizer, Ctrl) ->
    {Proportion, Border, Flags} = sizer_flags(What, wxBoxSizer:getOrientation(Sizer)),
    %% io:format("What ~p ~p => ~p ~n",[What, wxBoxSizer:getOrientation(Sizer), {Proportion, Border, Flags}]),
    wxSizer:add(Sizer, Ctrl, [{proportion, Proportion}, {border, Border}, {flag, Flags}]).

sizer_flags(label, ?wxHORIZONTAL)     -> {0, 0, ?wxALIGN_CENTER_VERTICAL};
sizer_flags(label, ?wxVERTICAL)       -> {1, 0, ?wxALIGN_CENTER_VERTICAL};
sizer_flags(separator, ?wxHORIZONTAL) -> {1, 5, ?wxALL bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags(separator, ?wxVERTICAL)   -> {0, 5, ?wxALL bor ?wxEXPAND};
sizer_flags(text, ?wxHORIZONTAL)      -> {1, 0, ?wxALIGN_CENTER_VERTICAL};
sizer_flags(slider, ?wxHORIZONTAL)    -> {2, 0, ?wxALIGN_CENTER_VERTICAL};
sizer_flags(slider, ?wxVERTICAL)      -> {0, 0, ?wxEXPAND};
sizer_flags(button, _)                -> {0, 0, ?wxALIGN_CENTER_VERTICAL};
sizer_flags(image, _)                 -> {0, 5, ?wxALL bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags(choice, _)                -> {0, 0, ?wxALIGN_CENTER_VERTICAL};
sizer_flags(checkbox, ?wxVERTICAL)    -> {0, 3, ?wxTOP bor ?wxBOTTOM bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags(checkbox, ?wxHORIZONTAL)  -> {0, 2, ?wxRIGHT bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags(table,  _)                -> {4, 0, ?wxEXPAND};
sizer_flags({radiobox, Dir}, Dir)     -> {5, 0, ?wxEXPAND bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags({radiobox, _}, _)         -> {1, 0, ?wxEXPAND bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags({box, Dir}, Dir)          -> {0, 2, ?wxALL bor ?wxEXPAND bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags({box, _}, _)              -> {0, 2, ?wxALL bor ?wxEXPAND bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags(custom, _)                -> {0, 5, ?wxALL};
sizer_flags(_, ?wxHORIZONTAL)         -> {1, 0, ?wxALIGN_CENTER_VERTICAL};
sizer_flags(_, ?wxVERTICAL)           -> {0, 0, ?wxEXPAND}.

create(false, _) -> undefined;
create(_, Fun) -> Fun().

tooltip(Ctrl, Flags) ->
    case proplists:get_value(info, Flags) of
	undefined -> ok;
	Str -> wxWindow:setToolTip(Ctrl, wxToolTip:new(Str))
    end.

to_str(Number) when is_integer(Number) ->
    integer_to_list(Number);
to_str(Float) when is_float(Float) ->
    wings_util:nice_float(Float);
to_str(List = [C|_]) when is_integer(C) ->
    List;
to_str([]) -> [].

pos(C, S) -> pos(C, S, 0).
pos(C, [C|_Cs], I) -> I;
pos(C, [_|Cs], I) -> pos(C, Cs, I+1);
pos(_, [], _I) -> 0.

image_to_bitmap(ImageOrFile) ->
    Img = case ImageOrFile of
	      File when is_list(File) ->
		  wxImage:new(File);
	      #e3d_image{} = E3D ->
		  wings_image:e3d_to_wxImage(E3D);
	      WxImage ->
		  wxImage = wx:getObjectType(WxImage), %% Assert
		  WxImage
	  end,
    BM = wxBitmap:new(Img),
    Img =:= ImageOrFile orelse wxImage:destroy(Img),
    BM.


text_to_html(Paragraphs) ->
    Header = ["<html>"],
    Html = text_to_html(Paragraphs, Header),
    lists:reverse(["</html>"|Html]).

text_to_html([[_|_] = Paragraph|Ps], Acc) ->
    text_to_html(Ps, ["</p>", paragraph_to_html(Paragraph), "<p>"|Acc]);
text_to_html([Table = {table,_,_,_}|Ps], Acc) ->
    text_to_html(Ps, [table_to_html(Table)|Acc]);
text_to_html([{bullet, List}|Rest], Acc) ->
    BulletList = ["</p><ul>",
		  ["<li>" ++ paragraph_to_html(Item) ++ "</li>" || Item <- List],
		  "</ul><p>"],
    text_to_html(Rest, [BulletList|Acc]);
text_to_html([], Acc) -> Acc.

paragraph_to_html([C|Text]) when is_integer(C) ->
    [C|paragraph_to_html(Text)];
paragraph_to_html([{bold, Text}|Rest]) ->
    ["<b>", paragraph_to_html(Text), "</b>"| paragraph_to_html(Rest)];
paragraph_to_html([{ul, Text}|Rest]) ->
    ["<u>", paragraph_to_html(Text), "</u>"| paragraph_to_html(Rest)];
paragraph_to_html([Table={table, _, _, _}|Rest]) ->
    ["</p>", table_to_html(Table), "<p>" | paragraph_to_html(Rest)];
paragraph_to_html([C|Text]) when is_list(C) ->
    [paragraph_to_html(C), paragraph_to_html(Text)];
paragraph_to_html([]) -> [].

table_to_html({table, _, Header, Items}) ->
    ["<p><b>", Header, "</b></p><table>",
     [table_row_to_html(Row) || Row <- Items],
     "</table>"].

table_row_to_html(Row) when is_list(Row) ->
    ["<tr>", ["<td>" ++ paragraph_to_html(Column) ++ "</td>" || Column <- Row], "</tr>"].

%%%%%%%%%%%%%%%%%%%%
validate(Fun, Input, Def) when is_number(Def) ->
    do_validate(Fun, Input, Def);
validate(_, Input, _) ->
    Input.
do_validate(Fun, Input, Def) ->
    case Fun(Input) of
	{true, Value} -> Value;
	false -> Def
    end.

validator(Val, Flags) when is_integer(Val) ->
    integer_validator(Flags);
validator(Val, Flags) when is_float(Val) ->
    float_validator(Flags);
validator(Val, _Flags) when is_list(Val) ->
    {30, fun(Str) -> {true, Str} end}.

integer_validator(Flags) ->
    case proplists:get_value(range, Flags) of
	undefined -> {8,accept_all_fun(integer)};
	{'-infinity',infinity} -> {8,accept_all_fun(integer)};
	{Min,infinity} when is_integer(Min) ->
	    {8,integer_range(Min, infinity)};
	{'-infinity',Max} when is_integer(Max) ->
	    {8,integer_range('-infinity', Max)};
	{Min,Max,Default} when is_integer(Min), is_integer(Max), is_integer(Default),
			       Min =< Default, Default =< Max ->
	    Digits = trunc(math:log(Max-Min+1)/math:log(10))+2,
	    {Digits,integer_range(Min, Max, Default)};
	{Min,Max} when is_integer(Min), is_integer(Max), Min =< Max ->
	    Digits = trunc(math:log(Max-Min+1)/math:log(10))+2,
	    {Digits,integer_range(Min, Max)}
    end.

float_validator(Flags) ->
    case proplists:get_value(range, Flags) of
	undefined -> {12,accept_all_fun(float)};
	{'-infinity',infinity} -> {12,accept_all_fun(float)};
	{Min,infinity} when is_float(Min) ->
	    {12,float_range(Min, infinity)};
	{'-infinity',Max} when is_float(Max) ->
	    {12,float_range('-infinity', Max)};
	{Min,Max,Default} when is_float(Min), is_float(Max), is_float(Default),
			       Min =< Default, Default =< Max ->
	    Digits = min(trunc(math:log(Max-Min+1)/math:log(10))+8, 20),
	    {Digits,float_range(Min, Max, Default)};
	{Min,Max} when is_float(Min), is_float(Max), Min =< Max ->
	    Digits = min(trunc(math:log(Max-Min+1)/math:log(10))+8, 20),
	    {Digits,float_range(Min, Max)}
    end.

integer_range(Min, Max, Default) ->
    fun(Str) ->
	    case eval_integer(Str) of
		error -> false;
		Int when Min =/= '-infinity', Int < Min ->
		    {true, Default};
		Int when Max =/= infinity, Int > Max ->
		    {true, Max};
		Int when is_integer(Int) -> {true, Int}
	    end
    end.

integer_range(Min, Max) ->
    fun(Str) ->
	    case eval_integer(Str) of
		error -> false;
		Int when Min =/= '-infinity', Int < Min ->
		    {true, Min};
		Int when Max =/= infinity, Int > Max ->
		    {true, Max};
		Int when is_integer(Int) -> {true, Int}
	    end
    end.

float_range(Min, Max, Default) ->
    fun(Str) ->
	    case eval_float(Str) of
		error -> false;
		Float when Min =/= '-infinity', Float < Min ->
		    {true, Default};
		Float when Max =/= infinity, Float > Max ->
		    {true, Default};
		Float when is_float(Float) -> {true, Float}
	    end
    end.

float_range(Min, Max) ->
    fun(Str) ->
	    case eval_float(Str) of
		error -> false;
		Float when Min =/= '-infinity', Float < Min ->
		    {true, Min};
		Float when Max =/= infinity, Float > Max ->
		    {true, Max};
		Float when is_float(Float) -> {true, Float}
	    end
    end.

accept_all_fun(integer) ->
    fun(Str) ->
	    case eval_integer(Str) of
		error -> false;
		Number -> {true, Number}
	    end
    end;
accept_all_fun(float) ->
    fun(Str) ->
	    case eval_float(Str) of
		error -> false;
		Number -> {true, Number}
	    end
    end.

%%%
%%%% History functions

type(Val) when is_integer(Val) -> int;
type(Val) when is_float(Val) -> float;
type(Val) when is_list(Val) -> string.

init_history() ->
    ets:new(wings_history, [named_table, public]),
    ets:insert(wings_history, {{string, next}, 0}),
    ets:insert(wings_history, {{float, next}, 0}),
    ets:insert(wings_history, {{int, next}, 0}).

use_history(#wx{event=#wxKey{keyCode=?WXK_UP}}, Type, Ctrl) ->
    read_hist(Type, -1, Ctrl);
use_history(#wx{event=#wxKey{keyCode=?WXK_DOWN}}, Type, Ctrl) ->
    read_hist(Type, 1, Ctrl);
use_history(#wx{event=#wxKey{controlDown=true, keyCode=$P}}, Type, Ctrl) ->
    read_hist(Type, -1, Ctrl);
use_history(#wx{event=#wxKey{controlDown=true, keyCode=$N}}, Type, Ctrl) ->
    read_hist(Type, 1, Ctrl);
use_history(#wx{event=_Key}, _, _) ->
    false.

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

%% reset_history() ->
%%     ets:delete(wings_history, {int,pos}),
%%     ets:delete(wings_history, {float,pos}),
%%     ets:delete(wings_history, {string,pos}).

read_hist(Type, Step, Ctrl) ->
    [{_, Next}] = ets:lookup(wings_history, {Type,next}),
    Curr0 = case ets:lookup(wings_history, {Type,pos}) of
		[] ->  Next;
		[{_,Key}] ->  Key
	    end,
    Curr1 = Curr0 + Step,
    if Curr1 < 0 ->
	    ets:insert(wings_history, {{Type,pos}, 0}),
	    false;
       Curr1 >= Next ->
	    ets:delete(wings_history,{Type,pos}),
	    false;
       true ->
	    ets:insert(wings_history, {{Type,pos}, Curr1}),
	    [{_,Val}] = ets:lookup(wings_history, {Type,Curr1}),
	    Prev = wxTextCtrl:getValue(Ctrl),
	    wxTextCtrl:setValue(Ctrl, Val),
	    {true, Prev}
    end.

%%%%%%%%%%%%%%%%%%%%%

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

fix_expr([], Acc)   -> lists:reverse(Acc, ".");
fix_expr([$.],Acc) -> lists:reverse(Acc, ".");
fix_expr([$.|T], [X|_]=Acc) when X >= $0, X =< $9 ->
    fix_expr(T, [$.|Acc]);
fix_expr([$.|T], Acc)  ->
    fix_expr(T, [$.,$0|Acc]);
%% Some math simplifications.
fix_expr("math:" ++ T, Acc) ->
    fix_expr(T, Acc);
fix_expr("pi" ++ T, Acc) ->
    fix_expr(T, lists:reverse("math:pi()", Acc));
%% Some extra functions.
fix_expr("deg2rad" ++ T, Acc) ->
    fix_expr(T, lists:reverse("(math:pi()/180)*", Acc));
fix_expr("rad2deg" ++ T, Acc) ->
    fix_expr(T, lists:reverse("(180/math:pi())*", Acc));
fix_expr([H|T],Acc) ->
    fix_expr(T, [H|Acc]).
