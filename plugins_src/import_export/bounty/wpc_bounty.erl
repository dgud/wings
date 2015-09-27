%%
%%  This file is part of TheBounty exporter for Wings3D,
%%  forked from YafaRay Wings3d exporter.
%%
%%  Copyright (C) 2013, 2015  Pedro Alcaide aka 'povmaniac' and others
%%
%%  See AUTHORS.txt file for more info about authors and license.
%%---------------------------------------------------------------------
%%  This program is free software; you can redistribute it and/or
%%  modify it under the terms of the GNU General Public License as
%%  published by the Free Software Foundation; either version 2 of
%%  the License, or (at your option) any later version.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
%%  See the  GNU General Public License for more details
%%  You should have received a copy of the GNU General Public License
%%  along with this program. If not, see <http://www.gnu.org/licenses/>.
%%

-module(wpc_bounty).
-export([init/0,menu/2,dialog/2,command/2]).

%% Debug exports
%% -export([now_diff_1/1]).

-include_lib("kernel/include/file.hrl").
-include("e3d.hrl").
-include("e3d_image.hrl").
-include("wings.hrl").

-import(lists, [reverse/1,reverse/2,sort/1,keydelete/3,
                foreach/2,foldl/3,foldr/3]).
-compile({no_auto_import,[max/2]}).

%%%
%% start split code to include files
-include("defines.erl").

%% Exported plugin callback functions
%%

init() ->
    init_pref(),
    set_var(rendering, false),
    true.

menu({file,export}, Menu) ->
    maybe_append(export, Menu, menu_entry(export));
menu({file,export_selected}, Menu) ->
    maybe_append(export, Menu, menu_entry(export));
menu({file,render}, Menu) ->
    maybe_append(render, Menu, menu_entry(render));
menu({edit,plugin_preferences}, Menu) ->
    Menu++menu_entry(pref);
menu(_, Menu) ->
    Menu.

command({file,{Op,?TAG}}, St) ->
    command_file(Op, St);
command({file,{Op,{?TAG,A}}}, St) ->
    command_file(Op, A, St);
command({edit,{plugin_preferences,?TAG}}, St) ->
    pref_dialog(St);
command(_Spec, _St) ->
    %% erlang:display({?MODULE,?LINE,_Spec}),
    next.

dialog({material_editor_setup,Name,Mat}, Dialog) ->
    maybe_append(edit, Dialog, material_dialog(Name, Mat));

dialog({material_editor_result,Name,Mat}, Res) ->
    case is_plugin_active(edit) of
        false -> {Mat,Res};
        _ -> material_result(Name, Mat, Res)
    end;

dialog({light_editor_setup,Name,Ps}, Dialog) ->
    case get_var(dialogs) of
        false-> Dialog;
        _ -> Dialog ++ [{?__(1,"TheBounty"), light_dialog(Name, Ps)}]
    end;

dialog({light_editor_result,Name,Ps0}, Res) ->
    case is_plugin_active(edit) of
        false -> {Ps0,Res};
        _ -> light_result(Name, Ps0, Res)
    end;

dialog(_X, Dialog) ->
    io:format("~p\n", [{_X,Dialog}]),
    Dialog.

%%
%% End of exported plugin callback functions

init_pref() ->
    Renderer = get_pref(renderer, ?DEF_RENDERER),
    RendererPath =
        case filename:pathtype(Renderer) of
            absolute ->
                Renderer;
            _ ->
                case wings_job:find_executable(Renderer) of
                    false -> false;
                    Path -> Path
                end
        end,
    case get_pref(dialogs, ?DEF_DIALOGS) of
        auto ->
            set_var(renderer, RendererPath),
            set_var(dialogs, case RendererPath of
                                 false -> false;
                                 _ -> true
                             end);
        enabled ->
            set_var(renderer, RendererPath),
            set_var(dialogs, true);
        disabled ->
            set_var(renderer, false),
            set_var(dialogs, false)
    end,
    ok.

maybe_append(Condition, Menu, PluginMenu) ->
    case {is_plugin_active(Condition),Menu} of
        {_,[plugin_manager_category]} -> Menu++PluginMenu;
        {false,_} -> Menu;
        {_,_} -> Menu++PluginMenu
    end.

is_plugin_active(Condition) ->
    case Condition of
        export -> get_var(dialogs);
        edit -> get_var(dialogs);
        render -> get_var(renderer)
    end.

menu_entry(export) ->
    [{?__(1,"TheBounty")++" (.xml)...",?TAG}];
menu_entry(_) ->
    %% render or pref
    [{?__(1,"TheBounty")++"...",?TAG}].

command_file(render=Op, _St) ->
    case get_var(rendering) of
        false ->
            export_dialog(Op, ?__(2,"TheBounty Render Options"));
        true ->
            wpa:error_msg(?__(1,"Already rendering."))
    end;
command_file(Op, _St) ->
    export_dialog(Op, ?__(3,"TheBounty Export Options")).

command_file(render, Attr, St) when is_list(Attr) ->
    set_prefs(Attr),
    do_export(export,
              props(render, Attr),
              [{?TAG_RENDER,true}|Attr], St);
command_file(Op, Attr, St) when is_list(Attr) ->
    %% when Op =:= export; Op =:= export_selected
    set_prefs(Attr),
    do_export(Op, props(Op, Attr), Attr, St).

-record(camera_info, {pos,dir,up,fov}).

do_export(Op, Props0, Attr0, St0) ->
    SubDiv = proplists:get_value(subdivisions, Attr0, ?DEF_SUBDIVISIONS),

    Props = [{subdivisions,SubDiv}|Props0],
    [{Pos,Dir,Up},Fov] = wpa:camera_info([pos_dir_up,fov]),
    CameraInfo = #camera_info{pos=Pos,dir=Dir,up=Up,fov=Fov},
    Attr = [CameraInfo,{lights,wpa:lights(St0)}|Attr0],
    ExportFun =
        fun (Filename, Contents) ->
            case catch export(Attr, Filename, Contents) of
                ok ->
                    ok;
                Error ->
                    io:format(?__(1,"ERROR: Failed to export")++":~n~p~n", [Error]),
                    {error,?__(2,"Failed to export")}
            end
        end,
    %% Freeze virtual mirrors.
    Shapes0 = gb_trees:to_list(St0#st.shapes),
    Shapes = [{Id,wpa:vm_freeze(We)} || {Id,We} <- Shapes0],
    St = St0#st{shapes=gb_trees:from_orddict(Shapes)},
    wpa:Op(Props, ExportFun, St).

props(render, Attr) ->
    RenderFormat =
        proplists:get_value(render_format, Attr, ?DEF_RENDER_FORMAT),
    {value,{RenderFormat,Ext,Desc}} =
        lists:keysearch(RenderFormat, 1, wings_job:render_formats()),
    Title =
        case os:type() of
            {win32,_} -> "Render";
            _Other    -> ?__(1,"Render")
        end,
    [{title,Title},{ext,Ext},{ext_desc,Desc}];

props(export, _Attr) ->
    {Title,File} =
        case os:type() of
            {win32,_} -> {"Export","TheBounty File"};
            _Other    -> {?__(2,"Export"),?__(3,"TheBounty File")}
        end,
    [{title,Title},{ext,".xml"},{ext_desc,File}];

props(export_selected, _Attr) ->
    {Title,File} =
        case os:type() of
            {win32,_} -> {"Export Selected","TheBounty File"};
            _Other    -> {?__(4,"Export Selected"),?__(5,"Bounty File")}
        end,
    [{title,Title},{ext,".xml"},{ext_desc,File}].


%%%
%%% Dialogues and results: Material
%%--------------------------------------------------------------------------------------------
-include("ui_material.erl").
%%--------------------------------------------------------------------------------------------

%% modulatrs def move to ui_material.erl


material_result(_Name, Mat0, Res0) ->
    %% take the Material settings
    {Ps1,Res1} = split_list(Res0,
        fun
            ({{?TAG,enabled,1},_}) -> true;   % look for the first modulator
            (_) -> false
        end),
    Ps2 = [{Key,Val} || {?KEY(Key),Val} <- Ps1],
    %% take the Modulators settings
    {Ps3,Res2} = modulator_result(Ps2, Res1),
    %% take the Object Parameters settings
    {Ps4,Res} = split_list(Res2,
        fun
            ({result,_}) -> true;   % look for the end of the list
            (_) -> false
        end),
    Ps = [{Key,Val} || {?KEY(Key),Val} <- Ps4] ++Ps3,
    Mat = [?KEY(Ps)|keydelete(?TAG, 1, Mat0)],
    {Mat,Res}.

%%-----------------------------
% split modulators code
%%-----------------------------
-include("ui_modulators.erl").

%%%
%%% Split Ligth dialogs
%!-----------------------------
-include("ui_lights.erl").
%------------------------------


pref_dialog(St) ->
    [{dialogs,Dialogs},{renderer,Renderer},{pluginspath,PluginsPath},
     {options,Options},{material_type,MatType}] =
        get_user_prefs([
            {dialogs,?DEF_DIALOGS},
            {renderer,?DEF_RENDERER},
            {pluginspath,?DEF_PLUGINS_PATH},
            {options,?DEF_OPTIONS},
            {material_type,?DEF_MATERIAL_TYPE}]),

    Dialog = [
        {vframe, [
            {hframe, [
                {menu,[
                    {?__(1,"Disabled Dialogs"),disabled},
                    {?__(2,"Automatic Dialogs"),auto},
                    {?__(3,"Enabled Dialogs"),enabled}
                ], Dialogs,[{key,dialogs}]},
                panel%, help_button(pref_dialog)
            ]},
            {label_column, [
                {?__(4,"Executable"),{button,{text,Renderer,[{key,renderer},{width,35},wings_job:browse_props()]}}},
                {?__(5,"TheBounty Plugins Path"),{button,{text,PluginsPath,[{key,pluginspath},{width,35},{props,[{dialog_type,dir_dialog}]}]}}},
                {?__(6,"Options"),{text,Options,[{key,options}]}},
                {?__(7,"Default Material"),{menu,menu_shader(), MatType, [{key,shader_type}]}}
            ]}
        ], [{title,""}]}],
    wpa:dialog(?__(8,"TheBounty Options"), Dialog, fun (Attr) -> pref_result(Attr,St) end).


pref_result(Attr, St) ->
    set_user_prefs(Attr),
    init_pref(),
    St.
%%%
%!-----------------------------
-include("ui_general.erl").
%!-----------------------------

%%% Export and rendering functions
%%%

export(Attr, Filename, #e3d_file{objs=Objs, mat=Mats, creator=Creator}) ->
    wpa:popup_console(),
    ExportTS = os:timestamp(),
    Render = proplists:get_value(?TAG_RENDER, Attr, false),
    KeepXML = proplists:get_value(keep_xml, Attr, ?DEF_KEEP_XML),
    RenderFormat =
        proplists:get_value(render_format, Attr, ?DEF_RENDER_FORMAT),
    ExportDir = filename:dirname(Filename),
    {ExportFile,RenderFile} =
        case {Render,KeepXML} of
            {true,true} ->
                {filename:rootname(Filename)++".xml", Filename};
            {true,false} ->
                {filename:join(ExportDir, ?MODULE_STRING++"-"
                               ++wings_job:uniqstr()++".xml"),
                 Filename};
            {false,_} ->
                {value,{RenderFormat,Ext,_}} =
                    lists:keysearch(RenderFormat, 1, wings_job:render_formats()),
                {Filename,filename:rootname(Filename)++Ext}
        end,
    F = open(ExportFile, export),
    io:format(?__(1,"Exporting  to:")++" ~s~n"++
              ?__(2,"for render to:")++" ~s~n", [ExportFile,RenderFile]),
    CreatorChg = re:replace(Creator,"-","_",[global]),
    CameraName = "x_Camera",
    BgName = "x_WorldBackground",
    Lights = proplists:get_value(lights, Attr, []),
    %%
    println(F,
        "<?xml version=\"1.0\"?>\n"
        "<!-- ~s: Exported from ~s -->\n"
        "<scene type=\"triangle\">\n",
         [filename:basename(ExportFile), CreatorChg]),

    %!----------------------
    % export shaders
    %!----------------------
    MatsGb =
        foldl(fun ({Name,Mat}, Gb) ->
                      export_shader(F, "w_"++format(Name), Mat, ExportDir),
                      println(F),
                      gb_trees:insert(Name, Mat, Gb)
              end, gb_trees:empty(), Mats),
    %%
    %*   MatsBlend =
    foldl(fun ({Name,Mat}, Gb) ->
                  export_shaderblend(F, "w_"++format(Name), Mat, ExportDir),
                  println(F),
                  gb_trees:insert(Name, Mat, Gb)
          end, gb_trees:empty(), Mats),


    %% Micheus Code for Meshlights Even Better
    section(F, "Objects"),
    foldr(fun (#e3d_object{name=Name,obj=Mesh}, Id) ->
                  export_object(F, "w_"++format(Name), Mesh, MatsGb, Id),
                  println(F),
                  Id+1
          end, 1, Objs),

    %!----------------------
    % export scene lights
    %!----------------------
    BgLights =
        reverse(
          foldl(fun ({Name,Ps}=Light, Bgs) ->
                        Bg = export_light(F, "w_"++format(Name), Ps),
                        println(F),
                        case Bg of
                            undefined -> Bgs;
                            _ -> [Light|Bgs]
                        end
                end, [], Lights)),
    %%
    %section(F, "Background, Camera, Filter and Render"),
    %!----------------------
    % environment background
    % TODO: need review
    %!----------------------
    warn_multiple_backgrounds(BgLights),
    %BgName =
    %    case BgLights of
    %        [] ->
    %            BgColor = proplists:get_value(background_color, Attr, ?DEF_BACKGROUND_COLOR),
    %            Ps = [{?TAG,[{background,constant},{background_color,BgColor}]}],
    %            export_background(F, ConstBgName, Ps),
    %            ConstBgName;
    %        [{Name,Ps}|_] ->
    %            N = "w_"++format(Name),
    %            export_background(F, N, Ps),
    %            N
    %    end,

    %% test for next background
    export_background(F, BgName, Attr), %(F, N, Ps),

    println(F),
    %!----------------------
    % export camera
    %!----------------------
    export_camera(F, CameraName, Attr),
    println(F),

    %% test:  split integrator code
    export_integrator(F, Attr),
    %!------------------------
    % export render options
    %!----------------------
    export_render(F, CameraName, BgName, filename:basename(RenderFile), Attr),
    %%
    println(F),
    println(F, "</scene>"),
    close(F),

    %!-------------------------
    %! Command line parameters
    %!-------------------------
    [{options,Options}] = get_user_prefs([{options,?DEF_OPTIONS}]),
    [{pluginspath,PluginsPath}] = get_user_prefs([{pluginspath,?DEF_PLUGINS_PATH}]),
        case {get_var(renderer),Render} of
            {_,false} ->
                wings_job:export_done(ExportTS),
                io:nl();
            {false,true} ->
                %% Should not happen since the file->render dialog
                %% must have been disabled
                if KeepXML -> ok;
                true -> file:delete(ExportFile) end,
            no_renderer;
        {_,true} when ExportFile == RenderFile ->
            export_file_is_render_file;
        {Renderer,true} ->
            SaveAlpha = proplists:get_value(save_alpha, Attr),
            AlphaChannel =  case SaveAlpha of
                                false -> " ";
                                _ ->
                                    " -a "
                            end,

            ArgStr = Options++case Options of
                                  [] -> [];
                                  _ -> " "
                              end
                ++wings_job:quote(filename:basename(ExportFile)),
            PortOpts = [{cd,filename:dirname(ExportFile)}],
            Handler =
                fun (Status) ->
                        if KeepXML -> ok; true -> file:delete(ExportFile) end,
                        set_var(rendering, false),
                        case Status of
                            ok -> {RenderFormat,RenderFile};
                            _  -> Status
                        end
                end,
            file:delete(RenderFile),
            set_var(rendering, true),
        Arguments = "-pp "++wings_job:quote(PluginsPath)++" "++AlphaChannel++"-f "++format(RenderFormat),
        wings_job:render(
                ExportTS,Renderer,Arguments++" "++ArgStr++" "++wings_job:quote(filename:rootname(Filename))++" ", PortOpts, Handler)
    end.

warn_multiple_backgrounds([]) ->
    ok;
warn_multiple_backgrounds([_]) ->
    ok;
warn_multiple_backgrounds(BgLights) ->
    io:format(?__(1,"WARNING: Multiple backgrounds")++" - ", []),
    foreach(fun ({Name,_}) ->
                    io:put_chars([format(Name), $ ])
            end, BgLights),
    io:nl(),
    ok.

section(F, Name) ->
    println(F, [io_lib:nl(),"<!-- Section ",Name," -->",io_lib:nl()]).


%%% Export Material code
-include("exp_material.erl").

%% Texture Export code
-include("exp_texture.erl").

%% split material modulators
-include("exp_modulators.erl").

% split geometry
-include("exp_geometry.erl").

% split export light code
-include("exp_light.erl").

-include("exp_camera.erl").

-include("exp_world.erl").

-include("exp_integrator.erl").

-include("exp_render.erl").


%%% Noisy file output functions. Fail if anything goes wrong.
%%%

open(Filename, export) ->
    case file:open(Filename, [write,raw,delayed_write]) of
        {ok, F} ->
            F;
        Error ->
            erlang:error(Error, [Filename, export])
    end.

println(F) ->
    println(F, "").

print(F, DeepString) ->
    case file:write(F, DeepString) of
        ok ->
            ok;
        Error ->
            erlang:error(Error, [F,DeepString])
    end.

println(F, DeepString) ->
    case file:write(F, [DeepString,io_lib:nl()]) of
        ok ->
            ok;
        Error ->
            erlang:error(Error, [F,DeepString])
    end.

print(F, Format, Args) ->
    case file:write(F, io_lib:format(Format, Args)) of
        ok ->
            ok;
        Error ->
            erlang:error(Error, [F,Format,Args])
    end.

println(F, Format, Args) ->
    case file:write(F, [io_lib:format(Format, Args),io_lib:nl()]) of
        ok ->
            ok;
        Error ->
            erlang:error(Error, [F,Format,Args])
    end.

close(F) ->
    case file:close(F) of
        ok ->
            ok;
        Error ->
            erlang:error(Error, [F])
    end.



%% Convert certain terms to printable strings in a a=\"1\"/>\n"
%% hopefully efficient way.

% test move declarations..
export_rgb(F, Type, {R,G,B,_}) ->
    export_rgb(F, Type, {R,G,B});
export_rgb(F, Type, {R,G,B}) ->
    println(F, ["\t<",format(Type)," r=\"",format(R),"\" g=\"",format(G),"\" b=\"",format(B),"\"/>"]).

% end

format(F) when is_float(F) ->
    I = abs(trunc(F)),
    D = abs(F) - float(I),
    if F < 0 ->
            [$-,integer_to_list(I)|format_decimals(D)];
       true ->
            [integer_to_list(I)|format_decimals(D)]
    end;
format(I) when is_integer(I) ->
    integer_to_list(I);
format(true) ->
    "true";
format(false) ->
    "false";
format(A) when is_atom(A) ->
    atom_to_list(A);
format(L) when is_list(L) ->
    L.

format_decimals(F) when is_float(F), F >= 0.0 ->
    format_decimals_1(F).

format_decimals_1(0.0) ->
    ".0";
format_decimals_1(F) when is_float(F) ->
    G = 10.0 * F,
    I = trunc(G),
    D = G - float(I),
    [$.,(I+$0)|format_decimals_2(D)].

format_decimals_2(0.0) ->
    [];
format_decimals_2(F) when is_float(F) ->
    G = 100.0 * F,
    I = trunc(G),
    D = G - float(I),
    if I < 10 ->
            [$0,(I+$0)|format_decimals_3(D)];
       true ->
            [integer_to_list(I)|format_decimals_3(D)]
    end.

format_decimals_3(0.0) ->
    [];
format_decimals_3(F) when is_float(F) ->
    G = 1000.0 * F,
    I = trunc(G),
    D = G - float(I),
    if I < 10 ->
            [$0,$0,(I+$0)|format_decimals_4(D)];
       I < 100 ->
            [$0,integer_to_list(I)|format_decimals_4(D)];
       true ->
            [integer_to_list(I)|format_decimals_4(D)]
    end.

format_decimals_4(0.0) ->
    [];
format_decimals_4(F) when is_float(F) ->
    G = 10000.0 * F,
    I = trunc(G),
    if I < 100 ->
            if I < 10 ->
                    [$0,$0,$0,(I+$0)];
               true ->
                    [$0,$0|integer_to_list(I)]
            end;
       true ->
            if I < 1000 ->
                    [$0|integer_to_list(I)];
               true ->
                    integer_to_list(I)
            end
    end.


%%% Set and get preference variables saved in the .wings file for this module
set_prefs(Attr) ->
    wpa:scene_pref_set(?MODULE, Attr).

set_user_prefs(Attr) ->
    wpa:pref_set(?MODULE, Attr).

get_pref(Key, [H|_]=KeyDefs) when is_tuple(H)  ->
    case proplists:lookup(Key,KeyDefs) of
        {Key, Def} -> Def;
        Def -> Def
    end,
    get_pref(Key,Def);
get_pref(Key, Def) ->
    [{Key,Val}] = get_prefs([{Key,Def}]),
    Val.

get_prefs(KeyDefs) when is_list(KeyDefs) ->
    get_prefs_1(KeyDefs, make_ref()).

get_prefs_1([], _Undefined) ->
    [];
get_prefs_1([{Key,Def}|KeyDefs], Undefined) ->
    [{Key,case wpa:scene_pref_get(?MODULE, Key, Undefined) of
              Undefined ->
                  wpa:pref_get(?MODULE, Key, Def);
              Val ->
                  Val
          end}|get_prefs_1(KeyDefs, Undefined)].

get_user_prefs(KeyDefs) when is_list(KeyDefs) ->
    [{Key,wpa:pref_get(?MODULE, Key, Def)} || {Key,Def} <- KeyDefs].

%% Set and get global variables (in the process dictionary)
%% per wings session for this module.

set_var(Name, undefined) ->
    erase_var(Name);
set_var(Name, Value) ->
    put({?MODULE,Name}, Value).

get_var(Name) ->
    get({?MODULE,Name}).

erase_var(Name) ->
    erase({?MODULE,Name}).

%%
% some useful declarations for User Interface
menu_shader() ->
    [{?__(1,"Shiny Diffuse"),shinydiffuse},
        {?__(2,"Glass"),glass},
        {?__(3,"Rough Glass"),rough_glass},
        {?__(4,"Glossy"),glossy},
        {?__(5,"Coated Glossy"),coatedglossy},
        {?__(6,"Translucent (SSS)"),translucent},
        {?__(7,"Light Material"),lightmat},
        {?__(8,"Blend"),blend_mat}].

%% Split a list into a list of length Pos, and the tail
%%
split_list(List, Pos) when is_list(List), is_integer(Pos), Pos >= 0 ->
    case split_list1(List, Pos, []) of
        {_,_}=Result -> Result;
        Error -> erlang:error(Error, [List, Pos])
    end;
split_list(List, Fun) when is_list(List), is_function(Fun) ->
    split_list2(List, Fun, []).
%%
split_list1(List, 0, Head) ->
    {lists:reverse(Head),List};
split_list1([], _Pos, _) ->
    badarg;
split_list1([H|T], Pos, Head) ->
    split_list1(T, Pos-1, [H|Head]).
%%
split_list2([H|T]=List, Fun, Head) ->
    case Fun(H) of
        true -> {lists:reverse(Head),List};
        _ -> split_list2(T, Fun, [H|Head])
    end.


%% Zip lists together into a list of tuples
%%
zip_lists([], []) -> [];
zip_lists([H1|T1], [H2|T2]) -> [{H1,H2}|zip_lists(T1, T2)].



%%% %% {lists:filter(Pred, List),lists:filter(fun(X) -> not Pred(X) end, List)}
%%% filter2(Pred, List) -> filter2_1(Pred, List, [], []).
%%% %%
%%% filter2_1(_Pred, [], True, False) ->
%%%     {reverse(True),reverse(False)};
%%% filter2_1(Pred, [H|T], True, False) ->
%%%     case Pred(H) of
%%%     true -> filter2_1(Pred, T, [H|True], False);
%%%     false -> filter2_1(Pred, T, True, [H|False])
%%%     end.

max(X, Y) when X > Y -> X;
max(_, Y) -> Y.



-ifdef(print_mesh_1).
print_mesh(#e3d_mesh{type=T,vs=Vs,vc=Vc,tx=Tx,ns=Ns,fs=Fs,he=He,matrix=M}) ->
    io:format("#e3d_mesh{type=~p,~nvs=~p,~nvc=~p,~ntx=~p,~nns=~p,~nfs=~p,~n"
              "he=~p,~nmatrix=~p}.~n",
              [T,Vs,Vc,Tx,Ns,Fs,He,M]).
-endif.
%%---------------------------------------------------
%% split tooltip help text to file
%%--------------------------------------------------
-include("ui_help.erl").

