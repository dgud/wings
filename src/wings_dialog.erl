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
	 dialog/3, dialog/4, dialog/5,
	 ask_preview/5, dialog_preview/5
	]).

%% Hook callbacks
-export([enable/3, show/3, update/2,
	 get_value/2, set_value/3,
	 get_widget/2]).

%% Internal dialogs
-export([return_result/3,
	 get_dialog_parent/0, set_dialog_parent/1, reset_dialog_parent/1]).

%%-compile(export_all).

-record(in, {key, type, def, wx, wx_ext=[], validator, data, hook, output=true}).
-record(eh, {fs, apply, prev_parent, owner, type, pid, dialog}).

%%
%% Syntax of Qs.
%%
%% Common types:
%%     String = string()
%%     Boolean = true | false
%%     Fields = [Field]
%%     Field  -- Any field tuple described below
%%     Key = term()
%%     Hook = function()
%%         Hook should have arity 3 and is invoked when something changes:
%%         fun (Key, Value, Fields) -> ok;
%%         end
%%
%% Qs is either one field below (preferably containing more fields,
%% or a list of fields, in which case the list is enclosed in a
%% {hframe,...} containing two {vframe,...}s, the first containing
%% Qs and the second containing an Ok and a Cancel button.
%%
%% Container fields (dialog tree nodes):
%%
%% {hframe,Fields[,Flags]}                      -- Horizontal frame
%% {vframe,Fields[,Flags]}                      -- Vertical frame
%%     Flags = [Flag]
%%     Flag = {title,String}|{minimized,Boolean}|{key,Key}|{hook,Hook}|layout|
%%            checkbox|invert
%% Only frames with both 'title' and 'minimized' flags return a value.
%% The 'checkbox' flag changes style of the frame to have a checkbox
%% field in the header indicating the minimized state. The 'invert'
%% flag makes the checkbox value and the return value of the field to
%% be the inverted minimized state (the maximized state ;-).
%%
%% {oframe,Fields[,Flags]}                      -- Overlay frame
%%     Flags = [Flag]
%%     Flag = {title,String}|{style,Style}|{key,Key}|{hook,Hook}|layout
%%     Style = menu|buttons  -- menu is default
%%
%%
%%
%% Composite fields (consisting of other fields)
%%
%% {vradio,Alts,DefaultValue[,Flags]}           -- Radio buttons vertically
%% {hradio,Alts,DefaultValue[,Flags]}           -- Radio buttons horizontally
%%     Alts = [{PromptString,Value}]
%%     Flags = [Flag]
%%     Flag = {key,Key}|{title,String}|{hook,Hook}
%% Example: see wpc_am.erl. These are {key,...} or {key_alt,...} fields
%% in a {hframe,...} or {vframe,...}.
%%
%% {label_column,Rows}                          -- Column of labeled fields
%%     Rows = [Row]
%%     Row = {String,Field}
%% This is a {hframe,...} containing one {vframe,...} with {label,String}
%% fields and one {vframe,Fields} containing the fields.
%%
%% {slider,{text,Def,Flags}}                    -- Slider on text field
%% {slider,{color,Col,Flags}}                   -- Slider on color box
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
%% panel                                        -- Blank filler, stretch 1
%% {panel,RegularFlags}
%% Does not return a value.
%%
%% {value,Value}                                -- Invisible value holder
%% {value,Value,RegularFlags}
%%
%% {label,String}                               -- Textual label
%% Does not return a value.
%%
%% separator                                    -- Horizontal separator line
%% Does not return a value.
%%
%% {color,Col[,RegularFlags]}                   -- Color box with sub-dialog
%%     Col = {R,G,B}|{R,G,B,A}
%%     R = G = B = A = X, 0.0 =< X, X =< 1.0
%%
%% {alt,Def,String,Val[,RegularFlags]}          -- Radiobutton
%%     Def = term()  -- Start value for radiobutton group.
%%                      One radiobutton in group must have this value.
%%     Val = term()  -- This button's value.
%%
%% {key_alt,{Key,Def},String,Val[,Flags]}       -- Radiobutton
%%    -> {alt,Def,String,Val,[{key,Key}|Flags]}
%%
%% {menu,Alts,Def[,RegularFlags]}               -- Pop-up menu
%%     Def = term()        -- Start value for menu.
%%                            One menu alternative must have this value.
%%     Alts = [Alt]
%%     Alt = {String,Val}|{String,Val,MenuFlags} -- Menu alt desc and value.
%%     Val = term()
%%     MenuFlags = [{info,String}]
%%
%% {button[,String],Action[,RegularFlags]}      -- Button
%%     Action = ok|preview|cancel|done|function(Result)
%% Only Action == done returns a value.
%% If String is omitted and Action is an atom, a String is
%% constructed by capitalizing the atom's string representation.
%% If Action is not an atom, String is mandatory.
%%
%% {custom,W,H,DrawFun[,RegularFlags]}          -- Custom look viewer
%%     W = H = integer()  -- Field size
%%     DrawFun = function(X, Y, W, H, Store)
%%         %% Should draw field and probably return 'keep'.
%%         %% Other return values are possible - read the source.
%%
%% {slider,{text,Def,Flags}}                    -- Slider on text field
%% {slider,{color,Col,Flags}}                   -- Slider on color box
%% {slider,Flags}                               -- Solo slider
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
%% {text,Def[,Flags]}                           -- Numerical or text field
%%     Def = integer()|float()|String  -- Start value
%%     Flags = {range,{Min,Max}}|{width,CharW}|{password,Bool}|
%%             {charset,Charset}|RegularFlag
%%     CharW = integer() >= 1
%%         %% Min and Max should be of same type as Def and is not
%%         %% allowed with a String field.
%%         %% Min can also be '-infinity' and Max 'infinity'.
%%         %% CharW is in characters. Default max-width is 300 for
%%         %% String fields, 10 for integer() and 10 for float()
%%         %% and the size is handled by the sizers.
%%         %% Charset is latin1 or unicode; default is latin1.
%%
%% {String,Bool[,RegularFlags]}                 -- Checkbox
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
    Parent = proplists:get_value(parent, Options, get_dialog_parent()),
    Flags  = [{size, {500, 400}}, {style, ?wxCAPTION bor ?wxRESIZE_BORDER bor ?wxCLOSE_BOX}],
    Frame  = wxMiniFrame:new(Parent, ?wxID_ANY, Title, Flags),
    Panel  = wxHtmlWindow:new(Frame, []),
    Sizer  = wxBoxSizer:new(?wxVERTICAL),
    Html = text_to_html(Info),
    wxHtmlWindow:appendToPage(Panel, Html),
    wxSizer:add(Sizer, Panel, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:setSizeHints(Sizer, Panel),
    wxWindow:setSizer(Frame, Sizer),
    wxFrame:show(Frame),
    wxScrolledWindow:setFocus(Panel),
    keep.

ask_preview(Cmd, Bool, Title, Qs, St) ->
    ask(Bool, Title, preview, Qs, preview_fun(Cmd, St)).

dialog_preview(Cmd, Bool, Title, Qs, St) ->
    dialog_1(Bool, Title, preview, Qs, preview_fun(Cmd, St)).

%% Currently a modal dialog
%%   (orginal wings let camera events trough)
ask(Title, Qs0, Fun) ->
    {PreviewCmd, Qs} = preview_cmd(Qs0),
    dialog_1(true, Title, PreviewCmd, queries(Qs), Fun).
ask(Ask, Title, Qs0, Fun) ->
    {PreviewCmd, Qs} = preview_cmd(Qs0),
    dialog_1(Ask, Title, PreviewCmd, queries(Qs), Fun).
ask(Ask, Title, PreviewCmd, Qs, Fun) ->
    dialog_1(Ask, Title, PreviewCmd, queries(Qs), Fun).

dialog(Title, Qs0, Fun) ->
    {PreviewCmd, Qs} = preview_cmd(Qs0),
    dialog_1(true, Title, PreviewCmd, Qs, Fun).
dialog(Title, Qs0, Fun, HelpFun) when is_list(Title)  ->
    {PreviewCmd, Qs} = preview_cmd(Qs0),
    dialog_1(true, Title, PreviewCmd, Qs, Fun, HelpFun);
dialog(Ask, Title, Qs0, Fun) ->
    {PreviewCmd, Qs} = preview_cmd(Qs0),
    dialog_1(Ask, Title, PreviewCmd, Qs, Fun).
dialog(Ask, Title, Qs0, Fun, HelpFun) ->
    {PreviewCmd, Qs} = preview_cmd(Qs0),
    dialog_1(Ask, Title, PreviewCmd, Qs, Fun, HelpFun).
dialog_1(Ask, Title, PreviewCmd, Qs0, Fun) when is_list(Qs0) ->
    Qs = {vframe_dialog, Qs0, [{buttons, [ok, cancel]}, {position, mouse}]},
    dialog_1(Ask, Title, PreviewCmd, Qs, Fun);
dialog_1(Ask, Title, PreviewCmd, Qs, Fun) when not is_list(Qs) ->
    case element(1,Qs) of
	preview -> error(Qs);
	drag_preview_cmd -> error(Qs);
	_Assert -> ok
    end,
    BuildDialog = case Ask of
                      true -> PreviewCmd;
                      false -> false;
                      return -> false
                  end,
    {Dialog, Fields} = build_dialog(BuildDialog, Title, Qs),
    %% io:format("Enter Dialog ~p ~p ~p~n",[Ask,PreviewCmd, Fields]),
    enter_dialog(Ask, PreviewCmd, Dialog, Fields, Fun).
dialog_1(Ask, Title, PreviewCmd, Qs0, Fun, HelpFun) when is_list(Qs0) ->
    Qs = {vframe_dialog, Qs0, [{buttons, [ok, cancel]},{help, HelpFun}, {position, mouse}]},
    dialog_1(Ask, Title, PreviewCmd, Qs, Fun).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Hook functions
enable(Keys = [_|_], Bool, Store) ->
    [enable(Key, Bool, Store) || Key <- Keys];
enable(Key, Bool, Store) ->
    [#in{wx=Ctrl, wx_ext=CtrlExt}] = ets:lookup(Store, Key),
    [wxWindow:enable(CtrlExt0,[{enable,Bool}]) || CtrlExt0 <- CtrlExt],
    wxWindow:enable(Ctrl, [{enable,Bool}]).

%%% Show/Hide a control. It's used for enable dynamics dialogs.
%%% After all controls has been shown/hidden we must call update/2
show(Key, Bool, Store) ->
    case ets:lookup(Store, Key) of
        [#in{wx=Ctrl, wx_ext=CtrlExt}] ->
            [wxWindow:show(CtrlExt0,[{show,Bool}]) || CtrlExt0 <- CtrlExt],
            wxWindow:show(Ctrl,[{show,Bool}]);
        _ -> ok
    end.

%%%

%%% Updates a control's sizer and all its children.
%%% For optimization purpose, it must be called after all calls to show/3 has been done.
%%% OBS: that is required due wxWidgets not to be propagating the updates (bug?!)
update(Key, Store) ->
    case ets:lookup(Store, Key) of
        [#in{wx=Ctrl}] -> update_children(Ctrl);
        _ -> ok
    end.
update_children([]) -> ok;
update_children([Ctrl|T]) ->
    update_children(T),
    update_children(Ctrl);
update_children(Ctrl) ->
    PSizer = wxWindow:getSizer(Ctrl),
    case wx:is_null(PSizer) of
        true -> ok;
        false ->
            update_children(wxWindow:getChildren(Ctrl)),
            wxSizer:layout(PSizer),
            wxWindow:fitInside(Ctrl)
    end.

get_widget(Key, Store) ->
    [#in{wx=Wx}] = ets:lookup(Store, Key),
    Wx.

get_value(Key, Store) ->
    try ets:lookup(Store, Key) of
        [In] -> get_curr_value(In)
    catch _:_ -> ok end.

set_value(Key, Value, Store) ->
    try ets:lookup(Store, Key) of
        [In] ->
            _ = set_value_impl(In, Value, Store),
            Store
    catch _:_ -> ok end.

set_value_impl(#in{wx=Ctrl, type=choice}, {Def, Entries}, _) when is_list(Entries) ->
    wxChoice:clear(Ctrl),
    lists:foldl(fun(Choice,N) -> setup_choices(Choice, Ctrl, Def, N) end, 0, Entries);
set_value_impl(#in{wx=Ctrl, type=choice}, Def, _) ->
    Count = wxChoice:getCount(Ctrl),
    SetDef = fun(N) ->
		     case wxChoice:getClientData(Ctrl, N) of
			 Def -> wxChoice:setSelection(Ctrl, N), true;
			 _ -> false
		     end
	     end,
    true = lists:any(SetDef, lists:seq(0, Count-1));
set_value_impl(#in{wx=Ctrl, type=text}, Val, _) ->
    wxTextCtrl:changeValue(Ctrl, to_str(Val));
set_value_impl(#in{wx=Ctrl, type=slider, data={_, ToSlider}}, Val, _) ->
    wxSlider:setValue(Ctrl, ToSlider(Val));
set_value_impl(#in{wx=Ctrl, type=col_slider}=In, Val, Store) ->
    ww_color_slider:setColor(Ctrl, Val),
    true = ets:insert(Store, In#in{data=Val});
set_value_impl(#in{wx=Ctrl, type=color, wx_ext=Ext}=In, Val, Store) ->
    case Ext of
	[Slider] ->
            ww_color_slider:setColor(Slider, Val);
	_ -> ignore
    end,
    ww_color_ctrl:setColor(Ctrl, Val),
    true = ets:insert(Store, In#in{data=Val});
set_value_impl(In=#in{type=button}, Val, Store) ->
    true = ets:insert(Store, In#in{data=Val});
set_value_impl(In=#in{type=value}, Val, Store) ->
    true = ets:insert(Store, In#in{data=Val});
set_value_impl(#in{wx=Ctrl, type=radiobox, data=Keys}, Val, _) ->
    Idx = get_list_index(Val,Keys),
    wxRadioBox:setSelection(Ctrl, Idx);
set_value_impl(#in{wx=Ctrl, type=checkbox}, Val, _) ->
    wxCheckBox:setValue(Ctrl, Val).

get_list_index(Val, List) ->
    get_list_index_0(Val, List, -1).

get_list_index_0(_, [], Idx) -> Idx;
get_list_index_0(Val, [Val|_], Idx) -> Idx+1;
get_list_index_0(Val, [_|T], Idx) ->
    get_list_index_0(Val, T, Idx+1).
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
    Rows = to_label_column(Qs0),
    [{label_column, Rows}].

%% ----------------------------------
%% We can only have one active dialog at the time but
%% they can open other dialog, i.e. color chooser from
%% preferences

get_dialog_parent() ->
    case ?GET(dialog_parent) of
	[Tuple|_] when element(1, Tuple) =:= wx_ref ->
	    Tuple;
        _ -> %% undefined or []
	    case wings_wm:this_win() of
		undefined -> ?GET(top_frame);
		Win -> Win
	    end
    end.

set_dialog_parent(Dialog) ->
    Prev = case ?GET(dialog_parent) of
               undefined  -> [];
               Stack -> Stack
           end,
    %io:format("Set parent ~p ~p~n",[Dialog,Prev]),
    ?SET(dialog_parent, [Dialog|Prev]).

reset_dialog_parent(Dialog) ->
    [Dialog|Stack] = ?GET(dialog_parent),
    %io:format("Reset parent ~p => ~p~n",[Dialog, Stack]),
    case Stack of
	[Next|_] ->
	    wxDialog:raise(Next);
	[] -> ok
    end,
    ?SET(dialog_parent, Stack).

enter_dialog(false, _, _, Fields, Fun) -> % No dialog return def values
    Values = get_output(default, Fields),
    true = ets:delete(element(1, Fields)),
    return_result(Fun, Values, wings_wm:this());
enter_dialog(return, _, _, Fields, Fun) -> % No dialog return def values
    Values = get_output(default, Fields),
    true = ets:delete(element(1, Fields)),
    return_result(fun(R) -> {return, Fun(R)} end, Values, wings_wm:this());
enter_dialog(true, no_preview, Dialog, Fields, Fun) -> %% No preview cmd / modal dialog
    set_dialog_parent(Dialog),
    case wxDialog:showModal(Dialog) of
	?wxID_CANCEL ->
	    reset_dialog_parent(Dialog),
	    wxDialog:destroy(Dialog),
	    true = ets:delete(element(1, Fields)),
	    keep;
	Result ->
	    Values = get_output(Result, Fields),
	    reset_dialog_parent(Dialog),
	    wxDialog:destroy(Dialog),
	    true = ets:delete(element(1, Fields)),
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
			     set_dialog_parent(Dialog),
			     wxDialog:show(Dialog),
			     wings_wm:psend(send_after_redraw, dialog_blanket, preview),
			     receive
				 closed ->
				     reset_dialog_parent(Dialog),
				     wxDialog:destroy(Dialog),
                                     true = ets:delete(element(1, Fields))
			     end
		     end),
    State = #eh{fs=Fields, apply=Fun, owner=wings_wm:this(),
		type=PreviewType, pid=Pid, dialog=Dialog},
    Op = {push,fun(Ev) -> event_handler(Ev, State) end},
    {TopW,TopH} = wings_wm:top_size(),
    wings_wm:new(dialog_blanket, {0,0,highest}, {TopW,TopH}, Op),
    wings_wm:grab_focus(dialog_blanket),
    keep.

notify_event_handler(false, _Msg) -> fun() -> ignore end;
notify_event_handler(no_preview, _) -> fun() -> ignore end;
notify_event_handler(_, Msg) -> fun() -> wings_wm:psend(send_once, dialog_blanket, Msg) end.

notify_event_handler_cb(false, _) -> fun(_,_) -> ignore end;
notify_event_handler_cb(no_preview, _) -> fun(_,_) -> ignore end;
notify_event_handler_cb(_, Msg) -> fun(_,_) -> wings_wm:psend(send_once, dialog_blanket, Msg) end.

event_handler(#wx{id=?wxID_CANCEL},
	      #eh{apply=Fun, owner=Owner, type=Preview, pid=Pid}) ->
    case Preview of
	preview ->
	    #st{}=St = Fun(cancel),
	    wings_wm:send(Owner, {update_state,St});
	drag_preview ->
	    wings_wm:send(Owner, cancel)
    end,
    Pid ! closed,
    delete;
event_handler(#wx{id=Result}=_Ev,
	      #eh{fs=Fields, apply=Fun, owner=Owner, pid=Pid}) ->
    %%io:format("Ev closing ~p~n  ~p~n",[_Ev, Fields]),
    Values = get_output(Result, Fields),
    Pid ! closed,
    try return_result(Fun, Values, Owner)
    catch throw:{command_error,Error} ->
	    wings_u:message(Error);
          _:Reason ->
            io:format("Dialog preview crashed: ~p~n~p~n",[Reason, erlang:get_stacktrace()])
    end,
    delete;
event_handler(preview, #eh{fs=Fields, apply=Fun, owner=Owner}) ->
    Values = get_output(preview, Fields),
    try Fun({dialog_preview,Values}) of
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
    catch _:Reason ->
            io:format("Dialog preview crashed: ~p~n~p~n",[Reason, erlang:get_stacktrace()]),
            keep
    end;
event_handler(#mousebutton{which=Obj}=Ev, _) ->
    wings_wm:send(wings_wm:wx2win(Obj), {camera,Ev,keep});
event_handler(#mousemotion{}, _) -> keep;
event_handler(got_focus, #eh{dialog=Dialog}) ->
    %% wxWidgets MacOSX workaround to keep dialog on top
    wxWindow:raise(Dialog),
    keep;
event_handler(_Ev, _) ->
    %% io:format("unhandled Ev ~p~n",[_Ev]),
    keep.

get_output(Result, {Table, Order}) ->
    Get = fun(Key, Acc) ->
		  case ets:lookup(Table, Key) of
		      [#in{output=false}] ->
			  Acc;
		      [#in{type=dialog_buttons} = In] ->
			  [with_key(In, result_atom(Result))|Acc];
		      [#in{def=Def} = In] when Result =:= default ->
			  [with_key(In, Def)|Acc];
		      [In] ->
			  [with_key(In, get_curr_value(In))|Acc]
		  end
	  end,
    lists:reverse(lists:foldl(Get, [], Order)).

get_curr_value(#in{type=checkbox, wx=Ctrl}) ->
    wxCheckBox:getValue(Ctrl);
get_curr_value(#in{type=radiobox, wx=Ctrl, data=Keys}) ->
    ZeroIndex = wxRadioBox:getSelection(Ctrl),
    lists:nth(ZeroIndex+1, Keys);
get_curr_value(#in{type=filepicker, wx=Ctrl}) ->
    wxFilePickerCtrl:getPath(Ctrl);
get_curr_value(#in{type=dirpicker, wx=Ctrl}) ->
    wxDirPickerCtrl:getPath(Ctrl);
get_curr_value(#in{type=fontpicker, wx=Ctrl}) ->
    wxFontPickerCtrl:getSelectedFont(Ctrl);
get_curr_value(#in{type=color, data=Data}) ->
    Data;
get_curr_value(#in{type=slider, wx=Ctrl, data={Convert,_}}) ->
    Convert(wxSlider:getValue(Ctrl));
get_curr_value(#in{type=col_slider, data=Val}) ->
    Val;
get_curr_value(#in{type=choice, wx=Ctrl}) ->
    wxChoice:getClientData(Ctrl,wxChoice:getSelection(Ctrl));
get_curr_value(#in{type=text, def=Def, wx=Ctrl, validator=Validate}) ->
    Str = wxTextCtrl:getValue(Ctrl),
    validate(Validate, Str, Def);
get_curr_value(#in{type=button, data=Data}) -> Data;
get_curr_value(#in{type=table, def=Def, wx=Ctrl}) ->
    Count = wxListCtrl:getItemCount(Ctrl),
    IsSelected =
	fun(N, Acc) ->
		case wxListCtrl:getItemState(Ctrl, N, ?wxLIST_STATE_SELECTED) of
		    ?wxLIST_STATE_SELECTED -> [N|Acc];
		    _ -> Acc
		end
	end,
    {lists:foldr(IsSelected, [], lists:seq(0, Count-1)), Def};
get_curr_value(#in{type=value, data=Data})  -> Data.


result_atom(?wxID_OK) -> ok;
result_atom(?wxID_CANCEL) -> cancel;
result_atom(?wxID_YES) -> yes;
result_atom(?wxID_NO) -> no.

with_key(#in{key=Integer}, Value) when is_integer(Integer) -> Value;
with_key(#in{key=Key}, Value) ->  {Key, Value}.

setup_hooks({Table, Keys}) ->
    _ = [setup_hook(hd(ets:lookup(Table, Key)), Table) || Key <- Keys],
    ok.

setup_hook(#in{key=Key, wx=Ctrl, type=color, wx_ext=Ext, hook=UserHook}, Fields) ->
    CB = fun({col_changed, Col}) ->
                 set_value(Key, Col, Fields),
		 UserHook =/= undefined andalso UserHook(Key, Col, Fields)
	 end,
    ww_color_ctrl:connect(Ctrl, col_changed, [{callback, CB}]),
    case Ext of
	[Slider] -> ww_color_slider:connect(Slider, col_changed, [{callback, CB}]);
	_ -> ignore
    end,
    ok;
setup_hook(#in{key=Key, wx=Ctrl, type=col_slider, hook=UserHook}, Fields) ->
    CB = fun({col_changed, Col}) ->
                 set_value(Key, Col, Fields),
                 UserHook =/= undefined andalso UserHook(Key, Col, Fields)
         end,
    ww_color_slider:connect(Ctrl, col_changed, [{callback, CB}]),
    ok;
%% The following only need callbacks if they contain user hooks
setup_hook(#in{hook=undefined}, _) -> ok;
setup_hook(#in{key=Key, wx=Ctrl, type=checkbox, hook=UserHook}, Fields) ->
    %% Setup callback
    wxWindow:connect(Ctrl, command_checkbox_clicked,
		     [{callback, fun(#wx{event=#wxCommand{commandInt=Int}}, Obj) ->
					 wxEvent:skip(Obj),
					 UserHook(Key, Int =/= 0, Fields)
				 end}]),
    %% And initiate
    UserHook(Key, wxCheckBox:getValue(Ctrl), Fields);
setup_hook(#in{key=Key, wx=Ctrl, type=choice, hook=UserHook}, Fields) ->
    wxWindow:connect(Ctrl, command_choice_selected,
		     [{callback, fun(#wx{event=#wxCommand{commandInt=Int}}, Obj) ->
					 wxEvent:skip(Obj),
					 Sel = wxChoice:getClientData(Ctrl, Int),
					 UserHook(Key, Sel, Fields)
				 end}]),
    UserHook(Key, wxChoice:getClientData(Ctrl,wxChoice:getSelection(Ctrl)), Fields);
setup_hook(#in{key=Key, wx=Ctrl, type=radiobox, hook=UserHook, data=Keys}, Fields) ->
    wxWindow:connect(Ctrl, command_radiobox_selected,
		     [{callback, fun(#wx{event=#wxCommand{commandInt=ZeroIndex}}, Obj) ->
					 wxEvent:skip(Obj),
					 Sel = lists:nth(ZeroIndex+1, Keys),
					 UserHook(Key, Sel, Fields)
				 end}]),
    UserHook(Key, lists:nth(1+wxRadioBox:getSelection(Ctrl),Keys),Fields);
setup_hook(#in{key=Key, wx=Ctrl, type=text, hook=UserHook, wx_ext=Ext, def=Def, validator=Validate}, Fields) ->
    wxWindow:connect(Ctrl, command_text_updated,
		     [{callback, fun(#wx{event=#wxCommand{cmdString=Str}}, Obj) ->
					 wxEvent:skip(Obj),
					 Val = validate(Validate, Str, Def),
					 UserHook(Key, Val, Fields),
					 ok
				 end}]),
    case Ext of
	[Slider] ->
        wxTextCtrl:connect(Ctrl, mousewheel,
                 [{callback, fun(#wx{event=#wxMouse{type=mousewheel}=EvMouse}, Obj) ->
                         wxEvent:skip(Obj),
                         Str = text_wheel_move(Def,wxTextCtrl:getValue(Ctrl),EvMouse),
                         case Validate(Str) of
                             {true, Val} ->
                                 UserHook(Key, Val, Fields),
                                 ok;
                             _ -> ok
                         end
                     end}]),
        wxSlider:connect(Slider, scroll_thumbtrack,
                 [{callback, fun(#wx{event=#wxScroll{commandInt=Val}}, Obj) ->
                         wxEvent:skip(Obj),
                         UserHook(Key, Val, Fields),
                         ok
                     end}]);
	_ -> ignore
    end,
    UserHook(Key,validate(Validate, wxTextCtrl:getValue(Ctrl), Def),Fields);
setup_hook(#in{key=Key, wx=Ctrl, type=button, hook=UserHook}, Fields) ->
    wxButton:connect(Ctrl, command_button_clicked,
		     [{callback, fun(_, _) ->
					 UserHook(Key, button_pressed, Fields)
				 end}]),
    ok;
setup_hook(#in{key=Key, wx=Ctrl, type=slider, hook=UserHook, data={FromSlider,_}}, Fields) ->
    wxSlider:connect(Ctrl, scroll_thumbtrack,
		     [{callback, fun(#wx{event=#wxScroll{commandInt=Val}}, _) ->
					 UserHook(Key, FromSlider(Val), Fields)
				 end}]),
    ok;

setup_hook(#in{key=Key, wx=Ctrl, type=fontpicker, hook=UserHook}, Fields) ->
    wxFontPickerCtrl:connect(Ctrl, command_fontpicker_changed,
			     [{callback, fun(_, Obj) ->
						 wxEvent:skip(Obj),
						 Font = wxFontPickerCtrl:getSelectedFont(Ctrl),
						 UserHook(Key, Font, Fields)
					 end}]),
    UserHook(Key, wxFontPickerCtrl:getSelectedFont(Ctrl), Fields);
setup_hook(#in{key=Key, wx=Ctrl, type=filepicker, hook=UserHook}, Fields) ->
    wxFilePickerCtrl:connect(Ctrl, command_filepicker_changed,
			    [{callback, fun(_, Obj) ->
						wxEvent:skip(Obj),
						Font = wxFilePickerCtrl:getPath(Ctrl),
						UserHook(Key, Font, Fields)
					end}]),
    UserHook(Key, wxFilePickerCtrl:getPath(Ctrl), Fields);

setup_hook(#in{key=Key, wx=Ctrl, type=dirpicker, hook=UserHook}, Fields) ->
    wxDirPickerCtrl:connect(Ctrl, command_dirpicker_changed,
			   [{callback, fun(_, Obj) ->
					       wxEvent:skip(Obj),
					       Font = wxDirPickerCtrl:getPath(Ctrl),
					       UserHook(Key, Font, Fields)
				       end}]),
    UserHook(Key, wxDirPickerCtrl:getPath(Ctrl), Fields);
setup_hook(#in{key=Key, wx=Ctrl, type=table, hook=UserHook}, Fields) ->
    wxListCtrl:connect(Ctrl, command_list_item_focused,
        [{callback, fun(_, _) ->
            UserHook(Key, Ctrl, Fields)
        end}]),
    ok;

%% Kind of special
setup_hook(#in{wx=Canvas, type=custom_gl, hook=CustomRedraw}, Fields) ->
    Env = wx:get_env(),
    Custom = fun() ->
                     try
                         wxGLCanvas:setCurrent(Canvas),
                         CustomRedraw(Canvas, Fields),
                         wxGLCanvas:swapBuffers(Canvas)
                     catch _:Reason ->
                             io:format("GL Custom callback crashed ~p~n~p~n",
                                       [Reason, erlang:get_stacktrace()])
                     end
	     end,
    Redraw = fun(#wx{}, _) ->
		     case os:type() of
                         {win32, _} -> DC=wxPaintDC:new(Canvas),
                                       wxPaintDC:destroy(DC);
                         _ -> false
                     end,
                     wx:set_env(Env),
                     wx:batch(Custom),
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
	    keep;
	{return, Result} ->
	    Result;
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
    end.

to_label_column(Qs) ->
    lists:map(fun label_col/1, Qs).
label_col({_Label,{menu,_,_}}=Entry) -> Entry;
label_col({_Label,{menu,_,_,_}}=Entry) -> Entry;
label_col({Label,Def}) -> {Label, {text,Def}};
label_col({Label,Def,Flags}) -> {Label, {text,Def, Flags}}.

build_dialog(false, _Title, Qs) ->
    {_,DialogData,_} = build(false, Qs, undefined, undefined),
    {undefined, DialogData};
build_dialog(AskType, Title, Qs) ->
    wx:batch(fun() ->
		     Parent = get_dialog_parent(),
		     Style0 = case os:type() of
				  %{unix, darwin} -> ?wxSTAY_ON_TOP;
				  _ -> ?wxFRAME_FLOAT_ON_PARENT
			      end,
		     Style  =  Style0 bor ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER,
		     Dialog = wxDialog:new(Parent, ?wxID_ANY, Title, [{style, Style}]),
                     %io:format("~p (~p) Parent ~p~n", [Title, Dialog, Parent]),
		     Panel  = wxPanel:new(Dialog, []),
		     wxPanel:setFont(Panel, ?GET(system_font_wx)),
		     Top    = wxBoxSizer:new(?wxVERTICAL),
		     Sizer  = wxBoxSizer:new(?wxVERTICAL),
		     try build(AskType, Qs, Panel, Sizer) of
			 {Location, DialogData, Fs} ->
			     set_keyboard_focus(Dialog, Fs),
			     wxWindow:setSizer(Panel, Sizer),
			     wxSizer:add(Top, Panel, [{proportion, 1},
						      {flag, ?wxEXPAND bor ?wxALL},
						      {border, 5}]),
			     setup_buttons(Dialog, Top, DialogData),
			     wxWindow:setSizerAndFit(Dialog, Top),
                             set_position(Location, Dialog),
			     setup_hooks(DialogData),
			     {Dialog, DialogData}
		     catch Class:Reason ->
			     %% Try to clean up
			     reset_dialog_parent(Dialog),
			     wxDialog:destroy(Dialog),
			     erlang:raise(Class, Reason, erlang:get_stacktrace())
		     end
	     end).

setup_buttons(Dialog, Top, {Table, Fields}) ->
    %% The dialog buttons are (should be) always last if present
    case ets:lookup(Table, lists:last(Fields)) of
	[DBs = #in{type=dialog_buttons, wx=Fun}] ->
	    Object = Fun(Dialog, Top),
	    true = ets:insert(Table, DBs#in{wx=Object}),
	    ok;
	_ ->
	    ok
    end.

set_keyboard_focus(Dialog, Fields) ->
    case lists:keyfind(text, #in.type, Fields) of
	#in{wx=Ctrl} ->
	    CB = fun(_, _) -> wxTextCtrl:setFocus(Ctrl) end,
	    wxDialog:connect(Dialog, show, [{callback, CB}]),
	    ok;
	false ->
	    ok
    end.

set_position(mouse, Dialog) ->
    {Xm,Ym} = wx_misc:getMousePosition(),
    {Wd, Hd} = wxWindow:getSize(Dialog),
    Ws = wxSystemSettings:getMetric(?wxSYS_SCREEN_X),
    Hs = wxSystemSettings:getMetric(?wxSYS_SCREEN_Y),
    {XWw, YWw} = wxWindow:getScreenPosition(?GET(top_frame)),
    if (Xm+Wd) < Ws, (Ym+Hd) < Hs ->
            wxWindow:move(Dialog, max(Xm-100, min(0,XWw)), max(Ym-50, min(0,YWw)));
       (Xm+Wd) < Ws ->
            wxWindow:move(Dialog, max(Xm-100, min(0,XWw)), max(Hs-Hd-50, min(0,YWw)));
       (Ym+Hd) < Hs ->
            wxWindow:move(Dialog, max(Ws-Wd-100, min(0,XWw)), max(Ym-50, min(0,YWw)));
       true ->
            io:format("~p ~p~n",[{Xm,Wd,Ws},{Ym,Hd,Hs}]),
            ok
    end;
set_position(center, Dialog) ->
    wxTopLevelWindow:centerOnScreen(Dialog);
set_position(_, _Dialog) ->
    ok.

build(Ask, Qs, Parent, Sizer) ->
    {Location,Fields} = build(Ask, Qs, Parent, Sizer, []),
    {Fs, _} = lists:mapfoldl(fun(In=#in{key=undefined},N) -> {In#in{key=N}, N+1};
				(In=#in{}, N) -> {In, N+1}
			     end, 1, lists:reverse(Fields)),
    Table = ets:new(?MODULE, [{keypos, #in.key}, public]),
    true = ets:insert(Table, Fs),
    {Location, {Table, [Key || #in{key=Key} <- Fs]}, Fs}.


build(Ask, {vframe_dialog, Qs, Flags}, Parent, Sizer, []) ->
    Def = proplists:get_value(value, Flags, ?wxID_OK),
    Location = proplists:get_value(position, Flags, {-1,-1}),
    Buttons = proplists:get_value(buttons, Flags, [ok, cancel]),
    HelpFun = proplists:get_value(help, Flags, undefined),
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
		     case HelpFun of
                         HelpFun when is_function(HelpFun)->
			     {Title,Content} = HelpFun(),
			     BtnHelp = wxButton:new(Dialog,?wxID_HELP),
			     wxSizer:insert(Ok, 0, BtnHelp, []),
			     Help = fun(#wx{},_) ->
					    info(Title,Content, [{parent, Dialog}])
				    end,
			     wxDialog:connect(Dialog, command_button_clicked,
			                      [{id, ?wxID_HELP}, {callback,Help}]);
		         _ -> ignore
		     end,

		     Ok
	     end,
    In = build(Ask, {vframe, Qs, [{proportion,1}]}, Parent, Sizer, []),
    {Location, [#in{key=proplists:get_value(key,Flags), def=Def,
		     output= undefined =/= proplists:get_value(key,Flags),
		     type=dialog_buttons, wx=Create}|In]};

build(Ask, {oframe, Tabs, Def, Flags}, Parent, WinSizer, In0)
  when Ask =/= false ->
    DY  =  wxSystemSettings:getMetric(?wxSYS_SCREEN_Y)*5 div 6, %% don't take entire screen.
    buttons =:= proplists:get_value(style, Flags, buttons) orelse error(Flags),
    NB = wxNotebook:new(Parent, ?wxID_ANY),
    AddPage = fun({Title, Data}, In) ->
		      Panel = wxScrolledWindow:new(NB, []),
		      wxScrolledWindow:setScrollbars(Panel,-1,10,-1,10), % no horizontal
		      wxWindow:setMaxSize(Panel, {-1,DY}),
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
    case 0 < Def andalso Def < length(Tabs) of
	true  -> wxNotebook:setSelection(NB, Def-1);
	false -> ignore
    end,
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
    add_sizer(label, Sizer, Text, Flags),
    MinSize = case proplists:get_value(min_wsz, Flags, -1) of
		  Sz when Sz > 0 -> Sz;
		  Sz ->
		      case proplists:get_value(width, Flags) of
			  undefined -> Sz;
			  Chars ->
			      {W, _, _, _} = wxWindow:getTextExtent(Parent, "W"),
			      Chars*W
		      end
	      end,
    wxSizer:setItemMinSize(Sizer, Text, MinSize, -1),
    In;
build(Ask, {label_column, Rows}, Parent, Sizer, In) ->
    build(Ask, {label_column, Rows, []}, Parent, Sizer, In);
build(Ask, {label_column, Rows, Flags}, Parent, Sizer, In) ->
    MinSize =
	if Ask =:= false -> -1;
	   true ->
		lists:foldl(fun(Tuple, Max) when is_tuple(Tuple) ->
				    String = [_|_] = element(1, Tuple),
				    {W, _, _, _} = wxWindow:getTextExtent(Parent, String),
				    max(W+5, Max);
			       (separator, Max)  ->
				    Max
			    end, -1, Rows)
	end,
    Fs = [{proportion, 0}],
    Translate = fun({String, Fields}) when is_list(Fields) ->
			{hframe, [{label, String, [{min_wsz, MinSize}]} | Fields], Fs};
		   ({String, Fields, LCFlags}) when is_list(Fields) ->
			{hframe, [{label, String, [{min_wsz, MinSize}]} | Fields], LCFlags ++ Fs};
		   ({String, Field}) ->
			{hframe, [{label, String, [{min_wsz, MinSize}]}, Field], Fs};
		   ({String, Field, LCFlags}) ->
			{hframe, [{label, String, [{min_wsz, MinSize}]}, Field], LCFlags ++ Fs};
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
    add_sizer(separator, Sizer, Separator, []),
    In;

build(Ask, {text, Def}, Parent, Sizer, In) ->
    build(Ask, {text, Def, []}, Parent, Sizer, In);
build(Ask, {text, Def, Flags}, Parent, Sizer, In) ->
    SizeVal = {_,Validator} = validator(Def, Flags),
    Create = fun() -> build_textctrl(Ask, Def, Flags, SizeVal, Parent, Sizer) end,
    [#in{key=proplists:get_value(key,Flags), def=Def,
	 hook=proplists:get_value(hook, Flags), type=text,
	 wx=create(Ask,Create), validator=Validator}|In];

build(Ask, {slider, {text, Def, Flags}}, Parent, Sizer, In) ->
    SizeVal = {_,Validator} = validator(Def, Flags),
    Create = fun() -> create_slider(Ask, Def, Flags, SizeVal, Parent, Sizer) end,
    {Ctrl,CtrlExt} = case create(Ask,Create) of
			 undefined -> {undefined, []};
			 Ctrls -> Ctrls
		     end,
    [#in{key=proplists:get_value(key,Flags), def=Def,
	 hook=proplists:get_value(hook, Flags),
	 type=text, wx=Ctrl, wx_ext=CtrlExt, validator=Validator}|In];

build(Ask, {slider, {color, Def, Flags}}, Parent, Sizer, In) ->
    Create = fun() ->
		     SS = wxBoxSizer:new(?wxHORIZONTAL),
		     SliderCtrl = ww_color_slider:new(Parent, ?wxID_ANY, Def),
		     ColCtrl = ww_color_ctrl:new(Parent, ?wxID_ANY, [{col, Def}]),

		     wxSizer:add(SS, SliderCtrl, [{proportion,2}, {flag, ?wxEXPAND}]),
		     wxSizer:add(SS, ColCtrl, [{proportion,0}]),
		     tooltip(SliderCtrl, Flags),
		     tooltip(ColCtrl, Flags),
		     add_sizer(slider, Sizer, SS, Flags),
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
		     {ColCtrl,[SliderCtrl]}
	     end,
    {Ctrl,CtrlExt} = case create(Ask,Create) of
			 undefined -> {undefined, []};
			 Ctrls -> Ctrls
		     end,
    [#in{key=proplists:get_value(key,Flags), def=Def,
	 hook=proplists:get_value(hook, Flags),
	 type=color, wx=Ctrl, wx_ext=CtrlExt, data=Def}|In];


build(Ask, {slider, Flags}, Parent, Sizer, In) ->
    Def = proplists:get_value(value, Flags),
    false = undefined == Def,
    case proplists:get_value(color, Flags, false) of
	false ->
	    Type = slider,
	    Range = proplists:get_value(range, Flags),
	    false = undefined == Range,
	    {Min, Value, Max, Style, ToText, ToSlider} = slider_style(Def, Range),
	    Data = {ToText, ToSlider},
	    Create = fun() ->
			     Ctrl = wxSlider:new(Parent, ?wxID_ANY, Value, Min, Max, [{style, Style}]),
			     tooltip(Ctrl, Flags),
			     add_sizer(slider, Sizer, Ctrl, Flags),
			     Ctrl
		     end;
	Color ->
	    true = lists:member(Color, [rgb, red, green, blue, hue, sat, val]), %% Assert
	    Type = col_slider,
            Data = Def,
	    Create = fun() ->
			     Ctrl = ww_color_slider:new(Parent, ?wxID_ANY, Def, [{color, Color}]),
			     tooltip(Ctrl, Flags),
			     add_sizer(slider, Sizer, Ctrl, Flags),
			     Ctrl
		     end
    end,
    [#in{key=proplists:get_value(key,Flags), def=Def, data=Data,
         hook=proplists:get_value(hook, Flags),
         type=Type, wx=create(Ask,Create)}|In];

build(Ask, {color, Def, Flags}, Parent, Sizer, In) ->
    Create = fun() ->
		     Static = proplists:get_value(static, Flags, false),
		     Dialog = proplists:get_value(native_dialog, Flags,
						  wings_pref:get_value(color_dialog_native)),
		     Ctrl = ww_color_ctrl:new(Parent, ?wxID_ANY,
					      [{col, Def},
					       {static, Static},
					       {native_dialog, Dialog}]),
		     tooltip(Ctrl, Flags),
		     add_sizer(button, Sizer, Ctrl, Flags),
		     Ctrl
	     end,
    [#in{key=proplists:get_value(key,Flags), def=Def, data=Def,
	 hook=proplists:get_value(hook, Flags),
	 type=color, wx=create(Ask,Create)}|In];

build(Ask, {button, {text, Def, Flags}}, Parent, Sizer, In) ->
    Props = proplists:get_value(props, Flags, []),
    DlgType = proplists:get_value(dialog_type, Props, open_dialog),
    CharWidth = wings_text:width("W"),
    case proplists:get_value(width, Flags, default) of
        Width when is_integer(Width), Width > 0 ->
            StyleEx = [{size, {CharWidth*(Width+1), -1}}];
        _ ->
            StyleEx = []
    end,

    if DlgType =/= dir_dialog ->
            Type = filepicker,
            Create = fun() ->
		            What = case DlgType of
		                       open_dialog -> ?wxFLP_OPEN;
		                       save_dialog -> ?wxFLP_SAVE;
		                       _ -> undefined
		                   end,
		            Filter = wings_file:file_filters(Props),
		            Ctrl = wxFilePickerCtrl:new(Parent, ?wxID_ANY,
		                [{style, What bor ?wxFLP_USE_TEXTCTRL},
		                    {path, Def},
		                    {wildcard, Filter}]++StyleEx),
		            PreviewFun = notify_event_handler_cb(Ask, preview),
		            wxFilePickerCtrl:connect(Ctrl, command_filepicker_changed,
						     [{callback, PreviewFun}]),
		            tooltip(Ctrl, Flags),
		            add_sizer(filepicker, Sizer, Ctrl, Flags),
		            Ctrl
		     end;
        true ->
            Type = dirpicker,
            Create = fun() ->
		            Ctrl = wxDirPickerCtrl:new(Parent, ?wxID_ANY,
		                [{style, ?wxDIRP_DEFAULT_STYLE bor ?wxDIRP_USE_TEXTCTRL}, {path, Def}]++StyleEx),
			    PreviewFun = notify_event_handler_cb(Ask, preview),
			    wxDirPickerCtrl:connect(Ctrl, command_dirpicker_changed,
						     [{callback, PreviewFun}]),
		            tooltip(Ctrl, Flags),
		            add_sizer(filepicker, Sizer, Ctrl, Flags),
		            Ctrl
		     end
    end,
    [#in{key=proplists:get_value(key,Flags), def=Def, hook=proplists:get_value(hook, Flags),
	 type=Type, wx=create(Ask,Create)}|In];

build(Ask, {button, Action}, Parent, Sizer, In)
  when is_atom(Action) ->
    build(Ask, {button, wings_util:cap(atom_to_list(Action)), Action, []},
	  Parent, Sizer, In);
build(Ask, {button, Action, Flags}, Parent, Sizer, In)
  when is_atom(Action) ->
    build(Ask,
	  {button, wings_util:cap(atom_to_list(Action)), Action, Flags},
	  Parent, Sizer, In);
build(Ask, {button, Label, Action}, Parent, Sizer, In) ->
    build(Ask, {button, Label, Action, []},  Parent, Sizer, In);
build(Ask, {button, Label, Action, Flags}, Parent, Sizer, In) ->
    Create = fun() ->
		     Ctrl = wxButton:new(Parent, ?wxID_ANY, [{label, Label}]),
		     tooltip(Ctrl, Flags),
		     add_sizer(button, Sizer, Ctrl, Flags),
		     Ctrl
	     end,

    Hook = case Action of
	       done ->
		   UserHook = proplists:get_value(hook, Flags),
		   fun(Key, button_pressed, Store) ->
			   wings_dialog:set_value(Key, true, Store),
			   UserHook == undefined orelse
			       UserHook(Key, button_pressed, Store)
		   end;
	       _ ->
		   proplists:get_value(hook, Flags)
	   end,
    [#in{key=proplists:get_value(key,Flags), def=false,
	 hook=Hook, data=false, output=Action=:=done,
	 type=button, wx=create(Ask,Create)}|In];


build(Ask, {fontpicker, DefFont, Flags}, Parent, Sizer, In) ->
    Def = case {(catch wx:getObjectType(DefFont) =:= wxFont), DefFont} of
	      {true,_}  -> DefFont;
	      {_, default} -> wxSystemSettings:getFont(?wxSYS_DEFAULT_GUI_FONT);
	      {_, FontInfo} when is_map(FontInfo) ->
		  wings_text:make_wxfont(FontInfo)
	  end,
    %% io:format("DefFont ~p: ~p~n",[DefFont, wxFont:getFaceName(Def)]),
    Create = fun() ->
		     PreviewFun = notify_event_handler_cb(Ask, preview),
		     Ctrl = wxFontPickerCtrl:new(Parent, ?wxID_ANY,
						 [{initial, Def},
						  {style, ?wxFNTP_FONTDESC_AS_LABEL}]),
		     wxFontPickerCtrl:connect(Ctrl, command_fontpicker_changed,
					      [{callback, PreviewFun}]),
		     tooltip(Ctrl, Flags),
		     add_sizer(fontpicker, Sizer, Ctrl, Flags),
		     Ctrl
	     end,
    [#in{key=proplists:get_value(key,Flags), def=Def,
	 hook=proplists:get_value(hook, Flags),
	 type=fontpicker, wx=create(Ask,Create)}|In];

build(Ask, {menu, Entries, Def, Flags}, Parent, Sizer, In) ->
    Create =
	fun() ->
		Ctrl = wxChoice:new(Parent, ?wxID_ANY),
		lists:foldl(fun(Choice,N) -> setup_choices(Choice, Ctrl, Def, N) end, 0, Entries),
		tooltip(Ctrl, Flags),
		add_sizer(choice, Sizer, Ctrl, Flags),
		Callback = {callback, notify_event_handler_cb(Ask, preview)},
		wxCheckBox:connect(Ctrl, command_choice_selected, [Callback]),
		Ctrl
	end,
    [#in{key=proplists:get_value(key,Flags), def=Def, hook=proplists:get_value(hook, Flags),
	 type=choice, wx=create(Ask,Create)}|In];

build(Ask, {table, [Header|Rows], Flags}, Parent, Sizer, In) ->
    Create =
	fun() ->
		Height = case proplists:get_value(max_rows, Flags) of
		             undefined -> -1;
		             MaxR -> max(MaxR+1, 5)*20
		         end,
		Widths = tuple_to_list(proplists:get_value(col_widths, Flags, {})),
		Style  = case proplists:get_value(sel_style, Flags) of
			     single -> ?wxLC_SINGLE_SEL bor ?wxLC_REPORT;
			     _ -> ?wxLC_REPORT
			 end,
		Options = [{style, Style}, {size, {-1, Height}}],
		Ctrl = wxListCtrl:new(Parent, Options),
		{CW, _, _, _} = wxWindow:getTextExtent(Ctrl, "D"),
		Li = wxListItem:new(),
		AddHeader = fun({HeadStr,W}, Column) ->
				    wxListItem:setText(Li, HeadStr),
				    wxListItem:setAlign(Li, ?wxLIST_FORMAT_RIGHT),
				    wxListCtrl:insertColumn(Ctrl, Column, Li),
				    wxListCtrl:setColumnWidth(Ctrl, Column, W*CW+10),
				    Column + 1
			    end,
		lists:foldl(AddHeader, 0, lists:zip(tuple_to_list(Header), Widths)),
		wxListItem:destroy(Li),

		Add = fun({_, Str}, {Row, Column}) ->
			      wxListCtrl:setItem(Ctrl, Row, Column, Str),
			      {Row, Column+1}
		      end,
		lists:foldl(fun(Row, N) ->
				    wxListCtrl:insertItem(Ctrl, N, ""),
				    lists:foldl(Add, {N, 0}, tuple_to_list(Row)),
				    N + 1
			    end, 0, Rows),
		add_sizer(table, Sizer, Ctrl, Flags),
		Ctrl
	end,
    [#in{key=proplists:get_value(key,Flags), def=Rows, hook=proplists:get_value(hook, Flags),
     	 type=table, wx=create(Ask,Create)}|In];

build(Ask, {image, ImageOrFile}, Parent, Sizer, In) ->
    Create = fun() ->
		     Bitmap = image_to_bitmap(ImageOrFile),
		     SBMap = wxStaticBitmap:new(Parent, ?wxID_ANY, Bitmap),
		     add_sizer(image, Sizer, SBMap, []),
		     wxBitmap:destroy(Bitmap),
		     SBMap
	     end,
    create(Ask, Create),
    In;

build(Ask, {help, Title, Fun}, Parent, Sizer, In) ->
    Display = fun(_,_) ->
		      info(Title, Fun(), [{parent, Parent}])
	      end,
    Create = fun() ->
		     Button = wxButton:new(Parent, ?wxID_HELP),
		     wxButton:connect(Button, command_button_clicked, [{callback, Display}]),
		     add_sizer(button, Sizer, Button, []),
		     Button
	     end,
    create(Ask,Create),
    In;

build(Ask, {custom_gl, CW, CH, Fun}, Parent, Sizer, In) ->
    build(Ask, {custom_gl, CW, CH, Fun, []}, Parent, Sizer, In);
build(Ask, {custom_gl, CW, CH, Fun, Flags}, Parent, Sizer, In) ->
    Context = wxGLCanvas:getContext(?GET(gl_canvas)),
    Create = fun() ->
		     Ps = [wings_gl:attributes(),{style, ?wxFULL_REPAINT_ON_RESIZE}],
		     Canvas = wxGLCanvas:new(Parent, Context, Ps),
                     wxGLCanvas:setMinSize(Canvas, {CW,CH}),
		     add_sizer(custom, Sizer, Canvas, Flags),
		     Canvas
	     end,
    [#in{key=proplists:get_value(key,Flags), type=custom_gl,
	 output=false, hook=Fun, wx=create(Ask, Create)}|In];

build(_Ask, {value, Def, Flags}, _Parent, _Sizer, In) ->
    [#in{type=value,
	 def=Def, data=Def,
	 key=proplists:get_value(key,Flags),
	 hook=proplists:get_value(hook, Flags)}|In];

build(Ask, {Label, Def}, Parent, Sizer, In) ->
    build(Ask, {Label, Def, []}, Parent, Sizer, In);
build(Ask, {Label, Def, Flags}, Parent, Sizer, In)
  when is_boolean(Def) ->
    Create = fun() ->
		     Ctrl = wxCheckBox:new(Parent, ?wxID_ANY, Label),
		     tooltip(Ctrl, Flags),
		     Callback = {callback, notify_event_handler_cb(Ask, preview)},
		     wxCheckBox:connect(Ctrl, command_checkbox_clicked, [Callback]),
		     wxCheckBox:setValue(Ctrl, Def),
		     add_sizer(checkbox, Sizer, Ctrl, Flags),
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
    Key = proplists:get_value(key, Flags, undefined),
    Margin = proplists:get_value(margin, Flags, undefined),
    Type0 =
	if Margin =:= false -> panel;
	   true -> {box, Type}
	end,
    case Key of
	undefined ->
	    Sizer = case proplists:get_value(title, Flags) of
			undefined -> wxBoxSizer:new(Type);
			Title -> wxStaticBoxSizer:new(Type, Parent, [{label, Title}])
		    end,
	    Input = lists:foldl(fun(Q, Input) ->
					build(Ask, Q, Parent, Sizer, Input)
				end, In0, Qs),
	    add_sizer(Type0, Top, Sizer, Flags),
	    Input;
	_ ->
	    Ctrl = wxPanel:new(Parent, [{style, ?wxNO_BORDER}]),
	    case os:type() of
		{win32,_} ->
		    wxPanel:setBackgroundColour(Ctrl, wxWindow:getBackgroundColour(Parent));
		_ -> ignore
	    end,
	    CSizer  = wxBoxSizer:new(Type),
	    In = lists:foldl(fun(Q, Input) ->
				     build(Ask, Q, Ctrl, CSizer, Input)
			     end, In0, Qs),
	    Show = proplists:get_value(show, Flags, undefined),
	    Enabled = proplists:get_value(enabled, Flags, undefined),
	    if Show =/= undefined ->
		    wxWindow:show(Ctrl,[{show,Show}]);
	       true -> ok
	    end,
	    wxWindow:setSizerAndFit(Ctrl,CSizer),
	    if Enabled =/= undefined ->
		    wxWindow:enable(Ctrl,[{enable,Enabled}]);
	       true -> ok
	    end,
	    wxWindow:setSizerAndFit(Ctrl,CSizer),
	    add_sizer(Type0, Top, Ctrl, Flags),
	    [#in{key=Key, type=panel, wx=Ctrl, output=false}|In]
    end.

build_radio(Ask, Def, {Dir, Style}, Alternatives, Flags, Parent, Sizer, In) ->
    Name = proplists:get_value(title, Flags, ""),
    {Strs,Keys} = lists:unzip(Alternatives),
    true = lists:member(Def, Keys),
    Create = fun() ->
		     Ctrl = wxRadioBox:new(Parent, 1, Name,
					   ?wxDefaultPosition, ?wxDefaultSize,
					   Strs, [{majorDim, 1}, {style, Style}]),
		     add_sizer({radiobox, Dir}, Sizer, Ctrl, Flags),
		     tooltip(Ctrl, Flags),
		     wxRadioBox:setSelection(Ctrl, pos(Def, Keys)),
		     Preview = notify_event_handler_cb(Ask, preview),
		     wxRadioBox:connect(Ctrl, command_radiobox_selected,
					[{callback, Preview}]),
		     Ctrl
	     end,
    [#in{key=proplists:get_value(key,Flags), hook=proplists:get_value(hook, Flags),
	 def=Def, type=radiobox, wx=create(Ask, Create),
	 data=Keys}|In].

build_textctrl(Ask, Def, Flags, {MaxSize, Validator}, Parent, Sizer) ->
    PreviewFun = notify_event_handler(Ask, preview),
    Ctrl = wxTextCtrl:new(Parent, ?wxID_ANY, [{value, to_str(Def)}]),
    {CharWidth,_,_,_} = wxWindow:getTextExtent(Ctrl, "W"),
    DefWidth = case proplists:get_value(proportion, Flags) of
		   undefined -> default;
		   _ -> undefined
	       end,
    case proplists:get_value(width, Flags, DefWidth) of
	default ->
	    wxTextCtrl:setMaxSize(Ctrl, {MaxSize*CharWidth, -1});
	Width when is_integer(Width) ->
	    wxTextCtrl:setMaxSize(Ctrl, {CharWidth*(Width+1), -1}),
	    wxTextCtrl:setMaxSize(Ctrl, {CharWidth*Width, -1});
	undefined -> %% Let the sizer handle the max and min sizes
	    ok
    end,
    tooltip(Ctrl, Flags),
    Type = type(Def),
    TextUpdated = fun(#wx{event=#wxCommand{cmdString=Str}},_) ->
			  case Validator(Str) of
			      {true, _} -> PreviewFun();
			      false -> ignore
			  end
		  end,
    case Type of
	string -> ignore;
        _ ->
	    UpdateTextWheel =
		fun(#wx{event=#wxMouse{type=mousewheel}=EvMouse}, _) ->
			Str = text_wheel_move(Def,wxTextCtrl:getValue(Ctrl),EvMouse),
			case Validator(Str) of
			    {true, Val} ->
				PreviewFun(),
				wxTextCtrl:setValue(Ctrl, to_str(Val));
			    _ ->
				ignore
			end
		end,
	    wxTextCtrl:connect(Ctrl, mousewheel, [{callback, UpdateTextWheel}])
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
    add_sizer(text, Sizer, Ctrl, Flags),
    Ctrl.

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


create_slider(Ask, Def, Flags, {MaxSize,Validator}, Parent, TopSizer) when is_number(Def) ->
    Range = proplists:get_value(range, Flags),
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    {Min, Value, Max, Style, ToText, ToSlider} = slider_style(Def, Range),
    Slider = wxSlider:new(Parent, ?wxID_ANY, Value, Min, Max, [{style, Style}]),
    Text = wxTextCtrl:new(Parent, ?wxID_ANY, [{value, to_str(Def)}]),
    {CharWidth,_,_,_} = wxWindow:getTextExtent(Text, "W"),
    wxTextCtrl:setMaxSize(Text, {MaxSize*CharWidth, -1}),
    wxSizer:add(Sizer, Slider, [{proportion,1}, {flag, ?wxEXPAND}]),
    wxSizer:add(Sizer, Text,   [{proportion,0}]),
    add_sizer(slider, TopSizer, Sizer, Flags),
    tooltip(Slider, Flags),
    tooltip(Text, Flags),
    case proplists:get_value(disable, Flags, false) of
	true -> wxWindow:disable(Slider),wxWindow:disable(Text);
	false -> ok
    end,

    PreviewFun = notify_event_handler(Ask, preview),
    UpdateText = fun(#wx{event=#wxCommand{commandInt=Where}}, _) ->
			 PreviewFun(),
			 wxTextCtrl:changeValue(Text, to_str(ToText(Where)))
		 end,
    wxSlider:connect(Slider, command_slider_updated, [{callback, UpdateText}]),
    UpdateTextWheel = fun(#wx{event=#wxMouse{type=mousewheel}=EvMouse}, _) ->
			  Str = text_wheel_move(Def,wxTextCtrl:getValue(Text),EvMouse),
			  case Validator(Str) of
			      {true, Val} ->
				  PreviewFun(),
				  wxSlider:setValue(Slider, ToSlider(Val)),
				  wxTextCtrl:changeValue(Text, to_str(Val));
			      _ ->
				  ignore
			  end
		      end,
    wxTextCtrl:connect(Text, mousewheel, [{callback, UpdateTextWheel}]),
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
    {Text,[Slider]}.

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
    {0, ToSlider(Def), 100, ?wxSL_HORIZONTAL, ToText, ToSlider};
slider_style(Def, {Min, Max}=MM) when Min < Max ->
    if Def < Min -> slider_style(Min,MM);
       Def > Max -> slider_style(Max,MM)
    end.

add_sizer(What, Sizer, Ctrl, Opts) ->
    {Proportion0, Border0, Flags0} =
	sizer_flags(What, wxBoxSizer:getOrientation(Sizer)),
    %% io:format("What ~p ~p => ~p ~n",[What, wxBoxSizer:getOrientation(Sizer), {Proportion, Border, Flags}]),
    Proportion = proplists:get_value(proportion, Opts, Proportion0),
    Border = proplists:get_value(border, Opts, Border0),
    Flags  = proplists:get_value(flag, Opts, Flags0),
    wxSizer:add(Sizer, Ctrl, [{proportion, Proportion},{border, Border},{flag, Flags}]).

sizer_flags(label, ?wxHORIZONTAL)      -> {0, 2, ?wxRIGHT bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags(label, ?wxVERTICAL)        -> {1, 2, ?wxRIGHT bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags(separator, ?wxHORIZONTAL)  -> {1, 5, ?wxALL bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags(separator, ?wxVERTICAL)    -> {0, 5, ?wxALL bor ?wxEXPAND};
sizer_flags(text, ?wxHORIZONTAL)       -> {1, 2, ?wxRIGHT bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags(slider, ?wxHORIZONTAL)     -> {2, 0, ?wxALIGN_CENTER_VERTICAL};
sizer_flags(slider, ?wxVERTICAL)       -> {0, 0, ?wxEXPAND};
sizer_flags(button, _)                 -> {0, 0, ?wxALIGN_CENTER_VERTICAL};
sizer_flags(image, _)                  -> {0, 5, ?wxALL bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags(choice, _)                 -> {0, 0, ?wxALIGN_CENTER_VERTICAL};
sizer_flags(checkbox, ?wxVERTICAL)     -> {0, 3, ?wxTOP bor ?wxBOTTOM bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags(checkbox, ?wxHORIZONTAL)   -> {0, 2, ?wxRIGHT bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags(table,  _)                 -> {4, 0, ?wxEXPAND};
sizer_flags({radiobox, Dir}, Dir)      -> {5, 0, ?wxEXPAND bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags({radiobox, _}, _)          -> {1, 0, ?wxEXPAND bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags({box, Dir}, Dir)           -> {0, 2, ?wxALL bor ?wxEXPAND bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags({box, _}, _)               -> {0, 2, ?wxALL bor ?wxEXPAND bor ?wxALIGN_CENTER_VERTICAL};
sizer_flags(fontpicker, ?wxHORIZONTAL) -> {2, 2, ?wxRIGHT};
sizer_flags(fontpicker, ?wxVERTICAL)   -> {0, 2, ?wxRIGHT bor ?wxEXPAND};
sizer_flags(filepicker, ?wxHORIZONTAL) -> {2, 2, ?wxRIGHT};
sizer_flags(filepicker, ?wxVERTICAL)   -> {0, 2, ?wxRIGHT bor ?wxEXPAND};
sizer_flags(custom, _)                 -> {0, 5, ?wxALL};
sizer_flags(panel, _)                  -> {0, 0, ?wxALL bor ?wxEXPAND bor ?wxALIGN_CENTER_VERTICAL}; %?wxEXPAND};
sizer_flags(_, ?wxHORIZONTAL)          -> {1, 0, ?wxALIGN_CENTER_VERTICAL};
sizer_flags(_, ?wxVERTICAL)            -> {0, 0, ?wxEXPAND}.

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
		  BlockWxMsgs = wxLogNull:new(),
		  Img0 = wxImage:new(File),
		  true = wxImage:ok(Img0), %% Assert
		  wxLogNull:destroy(BlockWxMsgs),
		  Img0;
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

text_to_html([{table,_,_,_}=Table|Ps], Acc) ->
    text_to_html(Ps, [table_to_html(Table)|Acc]);
text_to_html([{bullet, List}|Rest], Acc) ->
    BulletList = ["<ul>",
		  ["<li>" ++ paragraph_to_html(Item) ++ "</li>" || Item <- List],
		  "</ul>"],
    text_to_html(Rest, [BulletList|Acc]);
text_to_html([Paragraph|Ps], Acc) ->
    text_to_html(Ps, ["</p>", paragraph_to_html(Paragraph), "<p>"|Acc]);
text_to_html([], Acc) -> Acc.

paragraph_to_html([$\n|Text]) ->
    ["<br>",paragraph_to_html(Text)];
paragraph_to_html([C|Text]) when is_integer(C) ->
    [C|paragraph_to_html(Text)];
paragraph_to_html([{bold, Text}|Rest]) ->
    ["<b>", paragraph_to_html(Text), "</b>"| paragraph_to_html(Rest)];
paragraph_to_html([{ul, Text}|Rest]) ->
    ["<u>", paragraph_to_html(Text), "</u>"| paragraph_to_html(Rest)];
paragraph_to_html([Table={table, _, _, _}|Rest]) ->
    ["</p>", table_to_html(Table), "<p>" | paragraph_to_html(Rest)];
paragraph_to_html([{bullet, List}|Rest]) ->
    ["</p><ul>",
     ["<li>" ++ paragraph_to_html(Item) ++ "</li>" || Item <- List],
     "</ul><p>" | paragraph_to_html(Rest)];
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
    {300, fun(Str) -> {true, Str} end}.

integer_validator(Flags) ->
    case proplists:get_value(range, Flags) of
	undefined -> {10,accept_all_fun(integer)};
	{'-infinity',infinity} -> {10,accept_all_fun(integer)};
	{Min,infinity} when is_integer(Min) ->
	    {10,integer_range(Min, infinity)};
	{'-infinity',Max} when is_integer(Max) ->
	    {10,integer_range('-infinity', Max)};
	{Min,Max,Default} when is_integer(Min), is_integer(Max), is_integer(Default),
			       Min =< Default, Default =< Max ->
	    Digits = min(trunc(math:log(Max-Min+1)/math:log(10))+2,10),
	    {Digits,integer_range(Min, Max, Default)};
	{Min,Max} when is_integer(Min), is_integer(Max), Min =< Max ->
	    Digits = min(trunc(math:log(Max-Min+1)/math:log(10))+2,10),
	    {Digits,integer_range(Min, Max)}
    end.

float_validator(Flags) ->
    MaxDigits = proplists:get_value(digits, Flags, 10),
    case proplists:get_value(range, Flags) of
	undefined -> {MaxDigits,accept_all_fun(float)};
	{'-infinity',infinity} -> {MaxDigits,accept_all_fun(float)};
	{Min,infinity} when is_float(Min) ->
	    {MaxDigits,float_range(Min, infinity)};
	{'-infinity',Max} when is_float(Max) ->
	    {MaxDigits,float_range('-infinity', Max)};
	{Min,Max,Default} when is_float(Min), is_float(Max), is_float(Default),
			       Min =< Default, Default =< Max ->
	    Digits = min(trunc(math:log(Max-Min+1)/math:log(10))+8, MaxDigits),
	    {Digits,float_range(Min, Max, Default)};
	{Min,Max} when is_float(Min), is_float(Max), Min =< Max ->
	    Digits = min(trunc(math:log(Max-Min+1)/math:log(10))+8, MaxDigits),
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

constraint_factor(#wxMouse{altDown=A,shiftDown=S,metaDown=M,controlDown=R}) ->
    C = M or R,
    if
	C,S,A -> wings_pref:get_value(con_dist_ctrl_shift_alt);
	S,A -> wings_pref:get_value(con_dist_shift_alt);
	C,A -> wings_pref:get_value(con_dist_ctrl_alt);
	C,S -> wings_pref:get_value(con_dist_ctrl_shift);
	C -> wings_pref:get_value(con_dist_ctrl);
	S -> wings_pref:get_value(con_dist_shift);
	A -> wings_pref:get_value(con_dist_alt);
	true -> none
    end.

text_wheel_move(Def, Value, #wxMouse{wheelRotation=Count,wheelDelta=Delta}=EvMouse) ->
    Incr = case constraint_factor(EvMouse) of
	       none -> 1;
	       Other -> Other
	   end,
    try
	case is_integer(Def) of
	    true ->
		CurValue = list_to_integer(Value),
		Increment = round(Incr),
		integer_to_list(CurValue +round((Count/Delta)*Increment));
	    _ ->
		CurValue = try list_to_float(Value)
			   catch _:_ -> float(list_to_integer(Value))
			   end,
		Increment = Incr,
		float_to_list(CurValue +((Count/Delta)*Increment))
	end
    catch _:_ ->
	    Value
    end.
