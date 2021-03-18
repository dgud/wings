%%
%%  wings_view_win.erl --
%%
%%     View window.
%%
%%  Copyright (c) 2016 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wings_view_win).
-export([window/1,window/4,update/1]).

-export([init/1,
	 handle_call/3, handle_cast/2,
	 handle_event/2, % handle_sync_event/3,
	 handle_info/2, % code_change/3,
         terminate/2
	]).

-define(NEED_ESDL, true).
-include("wings.hrl").

-define(HEMI_LIGTH, 1000).
-define(HEMI_SKY, 1001).
-define(HEMI_GROUND, 1002).
-define(CAM_LIGHT, 1010).
-define(CAM_BG, 1011).
-define(CAM_BG_SLIDER, 1012).
-define(CAM_POSX, 1013).
-define(CAM_POSY, 1014).
-define(SCENE_LIGHT, 1020).

-define(AlignSz, 80).
-define(SPACER_SIZE, 8).

update(ChangedSetting) ->
    wings_wm:psend(?MODULE, ChangedSetting),
    ok.

window(St) ->
    case wings_wm:is_window(?MODULE) of
	true ->
	    wings_wm:raise(?MODULE),
	    keep;
	false ->
	    {DeskW, DeskH} = wings_wm:top_size(),
	    W = 28*?CHAR_WIDTH,
	    Pos  = {DeskW-50, 0},
	    Size = {W,DeskH div 4},
	    window(Pos, Size, [], St),
	    keep
    end.

window(Pos, Size, Ps0, St) ->
    Settings = get_init_state(St),
    {Frame,Ps} = wings_frame:make_win(title(), [{size, Size}, {pos, Pos}|Ps0]),
    Window = wings_sup:window(undefined, ?MODULE, [Frame, Ps, maps:to_list(Settings)]),
    Fs = [{display_data, geom_display_lists}|Ps],
    wings_wm:toplevel(?MODULE, Window, Fs, {push, change_state(Window, Settings)}),
    keep.

title() ->
    ?__(1,"View Settings").

get_init_state(#st{} = St) ->
    HaveSceneLight = have_scene_light(St),
    SceneLight = wings_pref:get_value(scene_lights, false),
    Light = use_light(HaveSceneLight, SceneLight),
    {CamX,CamY,_} = wings_pref:get_value(cl_lightpos),
    #{have_scene_light => HaveSceneLight,
      light => Light,
      cam_bg => wings_pref:get_value(show_bg),
      cam_bg_blur => wings_pref:get_value(show_bg_blur),
      cam_pos_x => CamX,
      cam_pos_y => CamY,
      hemi_sky => wings_pref:get_value(hl_skycol),
      hemi_ground => wings_pref:get_value(hl_groundcol)
     }.

have_scene_light(St) ->
    IsLight = fun(Obj) -> maps:is_key(light,Obj) andalso throw(true), Obj end,
    try
        wings_obj:map(IsLight,St),
        false
    catch true -> true
    end.

use_light(HaveSceneLight, SceneLight) ->
    CameraLight = wings_pref:get_value(number_of_lights) == 1,
    if SceneLight andalso HaveSceneLight -> scene_light;
       CameraLight -> camera_light;
       true -> hemi_light
    end.

changed_state(Key0, Val0, Window, State) ->
    case changed_state(Key0, Val0, State) of
        {true, {Key, Val} = Opt} ->
            ?dbg("Update ~p ~p~n",[Key,Val]),
            wx_object:cast(Window, {update, Opt}),
            {replace, change_state(Window, State#{Key:=Val})};
        {false, _What} ->
            ?dbg("Ignore: ~p~n",[_What]),
            keep
    end.

changed_state(number_of_lights, Val, #{light := Light0}) ->
    Light = case Val of
                1 -> camera_light;
                2 -> hemi_light
            end,
    {Light =/= Light0, {light, Light}};
changed_state(scene_lights, Bool, #{light:=Light0, have_scene_light:=HaveSceneLight}) ->
    Light = use_light(Bool, HaveSceneLight),
    {Light =/= Light0, {light, Light}};
changed_state(have_scene_light, Bool, #{have_scene_light:=HaveSceneLight}) ->
    {Bool =/= HaveSceneLight, {have_scene_light, Bool}};
changed_state(_Key, _Val, _State) ->
    ?dbg("Ignore: ~p ~p in ~p~n",[_Key, _Val, _State]),
    {false, ignored}.

change_state(Window, State) ->
    fun(Ev) -> forward_event(Ev, Window, State) end.

%% Forward to view_window
forward_event({current_state, St}, Window, State0) ->
    HaveSceneLight = have_scene_light(St),
    changed_state(have_scene_light, HaveSceneLight, Window, State0);
forward_event({view,Key,Val}, Window, State0) ->
    changed_state(Key, Val, Window, State0);
%% Forward to wings
forward_event({apply, {Light, radio}, _}, _Window, #{light := Prev}) ->
    case {Light, Prev} of
        {_, scene_light} ->
            CameraLight = wings_pref:get_value(number_of_lights) == 1,
            if Light =:= camera_light, CameraLight ->
                    wings_wm:send(geom, {action, {view, scene_lights}});
               Light =:= hemi_light, not CameraLight ->
                    wings_wm:send(geom, {action, {view, scene_lights}});
               true ->
                    wings_wm:send(geom, {action, {view, toggle_lights}})
            end;
        {scene_light, _} ->
            wings_wm:psend(geom, {action, {view, scene_lights}});
        {_, _} ->
            wings_wm:psend(geom, {action, {view, toggle_lights}})
    end,
    keep;
forward_event({apply, {hemi_light, sky}, RGB}, Window, State) ->
    wings_pref:set_value(hl_skycol, RGB),
    wings_wm:dirty(),
    change_state(Window, State#{hemi_sky := RGB});
forward_event({apply, {hemi_light, ground}, RGB}, Window, State) ->
    wings_pref:set_value(hl_groundcol, RGB),
    wings_wm:dirty(),
    change_state(Window, State#{hemi_ground := RGB});

forward_event({apply, {camera_light, pos_x}, Val}, Window, #{cam_pos_y := PosY} = State) ->
    wings_pref:set_value(cl_lightpos, {Val/2.0, PosY, 0.0}),
    wings_wm:dirty(),
    change_state(Window, State#{cam_pos_x := Val/2.0});
forward_event({apply, {camera_light, pos_y}, Val}, Window, #{cam_pos_x := PosX} = State) ->
    wings_pref:set_value(cl_lightpos, {PosX, Val/2.0, 0.0}),
    wings_wm:dirty(),
    change_state(Window, State#{cam_pos_y := Val/2.0});
forward_event({apply, {camera_opts, bg}, Bool}, Window, State) ->
    wings_pref:set_value(show_bg, Bool),
    wings_wm:dirty(),
    change_state(Window, State#{cam_bg := Bool});
forward_event({apply, {camera_opts, bg_slider}, Val}, Window, State) ->
    wings_pref:set_value(show_bg_blur, Val/100),
    wings_wm:dirty(),
    change_state(Window, State#{cam_bg_blur := Val});
forward_event(redraw, _, _) ->
    keep;
forward_event(_Ev, _Win, _State) ->
    ?dbg("Ignored: ~p~n",[_Ev]),
    keep.

init([Frame, _Ps, Os]) ->
    Panel = wxPanel:new(Frame),
    wxPanel:setFont(Panel, ?GET(system_font_wx)),
    #{bg:=BG} = _Cs = wings_frame:get_colors(),
    Szr = wxBoxSizer:new(?wxVERTICAL),
    wxPanel:setBackgroundColour(Panel, BG),

    BSz = 20,
    SubFlags = [{border, BSz}, {flag, ?wxEXPAND bor ?wxLEFT}],

    LightSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, ?__(1, "Light Settings")}]),
    HemL = create_hemilight(Panel, LightSz, SubFlags),
    CamL = create_cameralight(Panel, LightSz, SubFlags),
    SceneL = create_scenelight(Panel, LightSz, SubFlags),
    wxSizer:add(Szr, LightSz, [{border, 5}, {flag, ?wxEXPAND bor ?wxALL}]),

    CameraSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, ?__(2, "Camera Settings")}]),
    CamS = create_camera(Panel, CameraSz, SubFlags),
    wxSizer:add(Szr, CameraSz, [{border, 5}, {flag, ?wxEXPAND bor ?wxALL}]),

    wxPanel:setSizer(Panel, Szr),
    wxPanel:fit(Panel),
    wxWindow:fit(Frame),
    State0 = #{camera_light => CamL, hemi_light => HemL, scene_light => SceneL, camera_opts => CamS},
    State = ids_to_path(State0),
    wxWindow:connect(Panel, command_radiobutton_selected),
    wxWindow:connect(Panel, command_slider_updated),

    [setup_gui(Key,Val,State) || {Key,Val} <- Os],
    {Panel, State}.

create_hemilight(Panel, LightSz, SubFlags) ->
    HemiLB   = wxRadioButton:new(Panel, ?HEMI_LIGTH, ?__(2, "Hemilight"), [{style, ?wxRB_GROUP}]),
    Desc = ?__(3, "Simple light for modelling, only uses diffuse, emission and occlusion material properities, no highlights"),
    wxWindow:setToolTip(HemiLB, wxToolTip:new(Desc)),
    HemiSz   = wxBoxSizer:new(?wxVERTICAL),

    SkyCtrl    = ww_color_ctrl:new(Panel, ?HEMI_SKY, [{col, {1.0,1.0,1.0}}]),
    SkySz = pre_text(?__(4, "Sky:"), SkyCtrl, Panel),

    GroundCtrl = ww_color_ctrl:new(Panel, ?HEMI_GROUND, [{col, {0.2,0.2,0.2}}]),
    GroundSz = pre_text(?__(5, "Ground:"), GroundCtrl, Panel),

    wxSizer:add(LightSz, HemiLB),
    wxSizer:add(HemiSz, SkySz),
    wxSizer:add(HemiSz, GroundSz),
    wxSizer:add(LightSz, HemiSz, SubFlags),
    wxSizer:addSpacer(LightSz, ?SPACER_SIZE),
    ok = ww_color_ctrl:connect(SkyCtrl, col_changed),
    ok = ww_color_ctrl:connect(GroundCtrl, col_changed),
    #{radio => HemiLB, sz => HemiSz, sky=>SkyCtrl, ground=>GroundCtrl,
      ids => [{radio, ?HEMI_LIGTH}, {sky, ?HEMI_SKY}, {ground, ?HEMI_GROUND}]}.

create_cameralight(Panel, LightSz, SubFlags) ->
    CamLiLB = wxRadioButton:new(Panel, ?CAM_LIGHT, ?__(2, "Camera Light"), []),
    Desc = ?__(3, "Camera light, emulates a physically based renderer, uses all material properities"),
    wxWindow:setToolTip(CamLiLB, wxToolTip:new(Desc)),
    CamLiSz = wxBoxSizer:new(?wxVERTICAL),

    PosCtrlX = wxSlider:new(Panel, ?CAM_POSX, 0, -50, 50, [{style, ?wxSL_HORIZONTAL}]),
    wxWindow:setToolTip(PosCtrlX, wxToolTip:new(?__(41, "Camera lights horizontal offset from camera"))),
    PosSzX = pre_text(?__(4, "Horizontal:"), PosCtrlX, Panel),

    PosCtrlY = wxSlider:new(Panel, ?CAM_POSY, 0, -50, 50, [{style, ?wxSL_HORIZONTAL}]),
    wxWindow:setToolTip(PosCtrlY, wxToolTip:new(?__(51, "Camera lights vertical offset from camera"))),
    PosSzY = pre_text(?__(5, "Vertical:"), PosCtrlY, Panel),

    wxSizer:add(CamLiSz, PosSzX, [{flag, ?wxEXPAND}]),
    wxSizer:add(CamLiSz, PosSzY, [{flag, ?wxEXPAND}]),

    wxSizer:add(LightSz, CamLiLB),
    wxSizer:add(LightSz, CamLiSz, SubFlags),
    wxSizer:addSpacer(LightSz, ?SPACER_SIZE),
    #{radio => CamLiLB, sz => CamLiSz,
      x_slider => PosCtrlX, y_slider => PosCtrlY,
      ids => [{radio, ?CAM_LIGHT}, {pos_x, ?CAM_POSX}, {pos_y, ?CAM_POSY}]}.

create_scenelight(Panel, LightSz, SubFlags) ->
    SceneLB = wxRadioButton:new(Panel, ?SCENE_LIGHT, ?__(10, "Scene Light"), []),
    Desc = ?__(3, "Scene light, uses the scene lights, emulates a physically based renderer, uses all material properities"),
    wxWindow:setToolTip(SceneLB, wxToolTip:new(Desc)),

    SceneSz = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(LightSz, SceneLB),
    wxSizer:add(LightSz, SceneSz, SubFlags),
    wxSizer:addSpacer(LightSz, ?SPACER_SIZE),
    #{radio => SceneLB, sz => SceneSz,
      ids => [{radio, ?SCENE_LIGHT}]}.

create_camera(Panel, TopSz, SubFlags) ->
    SeeBg = wxCheckBox:new(Panel, ?CAM_BG, ?__(1, "View background"), [{style,?wxALIGN_LEFT}]),
    CamSz = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(CamSz, SeeBg),
    SeeBgBlur = wxSlider:new(Panel, ?CAM_BG_SLIDER, 30, 0, 100, [{style, ?wxSL_HORIZONTAL}]),
    wxWindow:setToolTip(SeeBgBlur, wxToolTip:new(?__(21, "Background blur"))),
    BlurSz = pre_text(?__(2, "Blur:"), SeeBgBlur, Panel),
    wxSizer:add(CamSz, BlurSz, [{flag, ?wxEXPAND}]),
    wxSizer:add(TopSz, CamSz, SubFlags),
    wxSizer:addSpacer(TopSz, ?SPACER_SIZE),
    wxCheckBox:connect(SeeBg, command_checkbox_clicked),
    #{bg => SeeBg, bg_slider => SeeBgBlur,
      ids => [{bg, ?CAM_BG}, {bg_slider, ?CAM_BG_SLIDER}]}.

pre_text(String, Ctrl, Parent) ->
    Sz = wxBoxSizer:new(?wxHORIZONTAL),
    Txt = wxStaticText:new(Parent, ?wxID_ANY, String),
    wxSizer:add(Sz, Txt, [{flag, ?wxALIGN_CENTER_VERTICAL}]),
    wxSizer:setItemMinSize(Sz, Txt, ?AlignSz, -1),
    wxSizer:add(Sz, Ctrl, [{proportion,1}]),
    Sz.

setup_gui(light, camera_light, #{camera_light:=Cam}) ->
    #{radio:=Button} = Cam,
    wxRadioButton:setValue(Button, true);
setup_gui(light, scene_light, #{scene_light:=Cam}) ->
    #{radio:=Button} = Cam,
    wxRadioButton:setValue(Button, true);
setup_gui(light, hemi_light, #{hemi_light:=Cam}) ->
    #{radio:=Button} = Cam,
    wxRadioButton:setValue(Button, true);
setup_gui(have_scene_light, Bool, #{scene_light:=Cam}) ->
    %% Scene light is available
    #{radio:=Button} = Cam,
    wxRadioButton:enable(Button, [{enable, Bool}]),
    ok;
setup_gui(cam_bg, Bool, #{camera_opts:=Cam}) ->
    #{bg:=OnOff, bg_slider:=Slider} = Cam,
    wxCheckBox:setValue(OnOff, Bool),
    wxSlider:enable(Slider, [{enable, Bool}]),
    ok;
setup_gui(cam_bg_blur, Val, #{camera_opts:=Cam}) ->
    #{bg_slider:=Slider} = Cam,
    wxSlider:setValue(Slider, round(Val*100)),
    ok;
setup_gui(cam_pos_x, Val, #{camera_light:=Cam}) ->
    #{x_slider:=Slider} = Cam,
    wxSlider:setValue(Slider, round(Val*2)),
    ok;
setup_gui(cam_pos_y, Val, #{camera_light:=Cam}) ->
    #{y_slider:=Slider} = Cam,
    wxSlider:setValue(Slider, round(Val*2)),
    ok;
setup_gui(hemi_sky, RGB, #{hemi_light:=Hemi}) ->
    #{sky:=Ctrl} = Hemi,
    ww_color_ctrl:setColor(Ctrl, RGB);
setup_gui(hemi_ground, RGB, #{hemi_light:=Hemi}) ->
    #{ground:=Ctrl} = Hemi,
    ww_color_ctrl:setColor(Ctrl, RGB);
setup_gui(_Key, _Val, _) ->
    ?dbg("Missing impl: ~p ~p~n",[_Key,_Val]),
    ok.


%%%%%%%%%%%%%

handle_event(#wx{id=?CAM_BG,event=#wxCommand{type=command_checkbox_clicked, commandInt = Val}=Ev},
             #{ids:=Ids, camera_opts:=Cam} = State) ->
    What = maps:get(?CAM_BG, Ids),
    forward_setting(What, Ev),
    #{bg_slider:=Slider} = Cam,
    wxSlider:enable(Slider, [{enable, Val == 1}]),
    {noreply, State};
handle_event(#wx{id=Id,event=Ev}, #{ids:=Ids} = State) ->
    What = maps:get(Id, Ids),
    forward_setting(What, Ev),
    {noreply, State};
handle_event(Ev, State) ->
    ?dbg("Unhandled ev: ~p~n",[Ev]),
    {noreply, State}.

handle_call(_Req, _From, State) ->
    ?dbg("~p:~p Got unexpected call ~p~n", [?MODULE,?LINE, _Req]),
    {reply, error, State}.

handle_cast({update, {Key, Val}}, State) ->
    setup_gui(Key, Val, State),
    {noreply, State};
handle_cast(_Req, State) ->
    ?dbg("~p:~p Got unexpected cast ~p~n", [?MODULE,?LINE, _Req]),
    {noreply, State}.

handle_info({col_changed, Id, _RGB} = Ev, #{ids:=Ids} = State) ->
    What = maps:get(Id, Ids),
    forward_setting(What, Ev),
    {noreply, State};
handle_info(parent_changed, State) ->
    %% Ignore
    {noreply, State};
handle_info(_Msg, State) ->
    ?dbg("~p:~p Got unexpected info ~p~n", [?MODULE,?LINE, _Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    wings ! {wm, {delete, ?MODULE}},
    normal.

%%%%%%%%%%%%%

forward_setting(What, #wxCommand{type=command_checkbox_clicked, commandInt = Val}) ->
    wings_wm:psend(?MODULE, {apply, What, Val == 1});
forward_setting(What, #wxCommand{type=command_slider_updated, commandInt = Val}) ->
    wings_wm:psend(?MODULE, {apply, What, Val});
forward_setting(What, {col_changed, _, RGB}) ->
    wings_wm:psend(?MODULE, {apply, What, RGB});
forward_setting(What, _Ev) ->
    wings_wm:psend(?MODULE, {apply, What, []}),
    ok.

ids_to_path(State) ->
    IdsToPath = [{Id, {Key1, Key2}} ||
                    {Key1, #{ids:=Ids}} <- maps:to_list(State),
                    {Key2, Id} <- Ids
                ],
    State#{ids => maps:from_list(IdsToPath)}.
