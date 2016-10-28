%%
%%  This file is part of TheBounty exporter for Wings3D 2.0.1 or above.
%%  Copyright (C) 2013-2016 Pedro Alcaide, aka povmaniac.
%%  Contact: thebountyrenderer@gmail.com
%%  See AUTHORS.txt for a complete list of authors.
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wpc_thebounty).
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
-include("thebounty/defines.erl").

%% Exported plugin callback functions
%%

init() ->
    ets:new(?LOCAL_MODULE, [named_table,public,ordered_set]),
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
    %maybe_append(edit, Menu, menu_entry(pref)); % new from micheus repo..
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
    SubDiv = proplists:get_value(subdivisions, Attr0, 0),

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
                    io:format(?__(1,"ERROR: Failed to write scene")++":~n~p~n", [Error]),
                    {error,?__(2,"Failed to write scene")}
            end
        end,
    %% Freeze virtual mirrors.
    Shapes0 = gb_trees:to_list(St0#st.shapes),
    Shapes = [{Id,wpa:vm_freeze(We)} || {Id,We} <- Shapes0],
    St = St0#st{shapes=gb_trees:from_orddict(Shapes)},
    wpa:Op(Props, ExportFun, St).

props(render, Attr) ->
    RenderFormat = proplists:get_value(render_format, Attr, png),
    {value,{RenderFormat,Ext,Desc}} =  lists:keysearch(RenderFormat, 1, wings_job:render_formats()),
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
            _Other    -> {?__(4,"Export Selected"),?__(5,"TheBounty File")}
        end,
    [{title,Title},{ext,".xml"},{ext_desc,File}].


%%%
%%% Dialogues and results: Material
%%--------------------------------------------------------------------------------------------
-include("thebounty/ui_material.erl").
%%--------------------------------------------------------------------------------------------

%% modulators def move to ui_material.erl

material_result(_Name, Mat0, Res) ->
    %% take the Material settings
    {Found0, Remaining} = rip_all(?TAG, Res),
    {Mod, Mat} = process_modulator(Found0),
    NewMat = [{?TAG, Mat++Mod} | lists:keydelete(?TAG, 1, Mat0)],
    {NewMat, Remaining}.

%%-----------------------------
% split modulators code
%%-----------------------------
-include("thebounty/ui_modulators.erl").

%%%
%%% Split Ligth dialogs
%!-----------------------------
-include("thebounty/ui_lights.erl").
%------------------------------
% for default material in preferences
menu_shader() ->
    [{?__(1,"Shiny Diffuse"),shinydiffuse},
    {?__(2,"Glass (Rough)"),glass},
    {?__(4,"Glossy (Coated)"),glossy},
    {?__(6,"Translucent (SSS)"),translucent},
    {?__(8,"Blend"),blend_mat}].

pref_dialog(St) ->
    [{dialogs,Dialogs},{renderer,Renderer},
     {options,Options},{material_type,DefaultMaterialType}] =
        get_user_prefs([
            {dialogs,?DEF_DIALOGS},
            {renderer,?DEF_RENDERER},
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
                panel, help_button(pref_dialog)
            ]},
            {label_column, [
                {?__(4,"Binary folder"),
                    {button,{text,Renderer,[{key,renderer},{width,35},{props,[{dialog_type,dir_dialog}]}]}}},
                {?__(6,"Options"),
                    {text,Options,[{key,options}]}},
                {?__(7,"Default Material"),
                    {menu,menu_shader(), DefaultMaterialType, [{key,default_material_type}]}}
            ]}
        ], [{title,""}]}],
    wpa:dialog(?__(8,"TheBounty Options"), Dialog, fun (Attr) -> pref_result(Attr,St) end).


pref_result(Attr, St) ->
    set_user_prefs(Attr),
    OldVal = get_var(renderer), % from Micheus
    init_pref(),
    %% more..
    case get_var(renderer) of
        OldVal -> ok;
        false ->
            wings_menu:update_menu(file, {render, ?TAG}, delete);
        _ ->
            [{Label, _}] = menu_entry(render),
            wings_menu:update_menu(file, {render, ?TAG}, {append, -1, Label})
    end,
    St.

%%%
%!-----------------------------
-include("thebounty/ui_general.erl").
%!-----------------------------


%%% Export and rendering functions
%%%
export(Attr, XMLFilename, #e3d_file{objs=Objs, mat=Mats, creator=Creator}) ->
    wpa:popup_console(),
    ExportTS = os:timestamp(),
    Render = proplists:get_value(?TAG_RENDER, Attr, false),
    KeepXML = proplists:get_value(keep_xml, Attr, false),
    GuiMode = proplists:get_value(gui_mode, Attr, false),
    RenderFormat = proplists:get_value(render_format, Attr, png),
    ExportDir = filename:dirname(XMLFilename),
    {ExportFile,RenderFile} =
        case {Render,KeepXML} of
            {true,true} ->
                {filename:rootname(XMLFilename)++".xml", XMLFilename};
            {true,false} ->
                {filename:join(ExportDir, ?MODULE_STRING++"-"++wings_job:uniqstr()++".xml"), XMLFilename};
            {false,_} ->
                {value,{RenderFormat,Ext,_}} =
                    lists:keysearch(RenderFormat, 1, wings_job:render_formats()),
                {XMLFilename,filename:rootname(XMLFilename)++Ext}
        end,
    F = open(ExportFile, export),
    io:format(?__(1,"Exporting  to:")++" ~s~n"++
              ?__(2,"for render to:")++" ~s~n", [ExportFile,RenderFile]),
    CreatorChg = re:replace(Creator,"-","_",[global]),
    %
    Lights = proplists:get_value(lights, Attr, []),
    %%
    println(F,
        "<?xml version=\"1.0\"?>\n"
        "<!-- ~s: Exported from ~s -->\n"
        "<scene type=\"triangle\">\n",
         [filename:basename(ExportFile), CreatorChg]),

    %!----------------------
    % write shaders
    %!----------------------
    write_default_materials(F),
    MatsGb =
        foldl(fun ({Name,Mat}, Gb) ->
                      export_shader(F, "w_"++format(Name), Mat, ExportDir),
                      println(F),
                      gb_trees:insert(Name, Mat, Gb)
              end, gb_trees:empty(), Mats),
    %%
    %*  MatsBlend =
    foldl(fun ({Name,Mat}, Gb) ->
                  export_shaderblend(F, format(Name), Mat, ExportDir),
                  %export_shaderblend(F, "w_"++format(Name), Mat, ExportDir), % old..
                  println(F),
                  gb_trees:insert(Name, Mat, Gb)
          end, gb_trees:empty(), Mats),


    %% Micheus Code for Meshlights Even Better
    section(F, "Objects"),
    foldr(fun (#e3d_object{name=Name,obj=Mesh}, Id) ->
                  export_object(F, format(Name), Mesh, MatsGb, Id),
                  println(F),
                  Id+1
          end, 1, Objs),

    %!----------------------
    % write scene lights
    %!----------------------
    %BgLights =
    reverse(
        foldl(fun ({Name,Ps}=Light, Bgs) ->
                Bg = export_light(F, "w_"++format(Name), Ps),
                println(F),
                case Bg of
                    undefined -> Bgs;
                    _ -> [Light|Bgs]
                end
            end, [], Lights)),

    %!------------------------
    %! environment background
    %!------------------------
    export_background(F, Attr),
    println(F),

    %!----------------------
    %! write camera
    %!----------------------
    export_camera(F, Attr),
    println(F),

    %!----------------------
    %! write integrators
    %!----------------------
    export_integrator(F, Attr),

    %!------------------------
    %! write render options
    %!------------------------
    export_render(F, filename:basename(RenderFile), Attr),
    %%
    println(F),
    println(F, "</scene>"),
    close(F),

    %!-------------------------
    %! Command line parameters
    %!-------------------------
    [{options,Options}] = get_user_prefs([{options,?DEF_OPTIONS}]),
    case {get_var(renderer),Render} of
        {_,false} ->
            wings_job:export_done(ExportTS),
            io:nl();
        {false,true} ->
            %% Should not happen since the file->render dialog must have been disabled
            if KeepXML -> ok;
                true -> file:delete(ExportFile) end,
            no_renderer;
        {_,true} when ExportFile == RenderFile ->
            export_file_is_render_file;
        {Renderer,true} ->
            SaveAlpha = proplists:get_value(save_alpha, Attr),
            AlphaChannel =
                case SaveAlpha of
                    true -> " -a ";
                    _ ->    " "
                end,

            ArgStr = Options++case Options of
                                  [] -> [];
                                  _ ->  " "
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
            % Set binarie file under each OS
            RenderExec =
                case os:type() of
                    {win32,_} ->
                        case GuiMode of
                            true -> format(Renderer)++"/thebounty-gui.exe";
                            _ -> format(Renderer)++"/thebounty-xml.exe"
                        end;
                    % atm, not GUI option under Linux or OSX
                    _ -> format(Renderer)++"/thebounty-xml"
                end,
            PluginsPath = "-pp "++wings_job:quote(format(Renderer)++"/plugins"),
            OutputFormat =
                case RenderFormat of
                    "" -> "-f png";
                    _ -> "-f "++format(RenderFormat)
                end,
            file:delete(RenderFile),
            set_var(rendering, true),
        Arguments = PluginsPath++AlphaChannel++OutputFormat,
        OutFile = wings_job:quote(filename:rootname(XMLFilename)),
        wings_job:render(ExportTS,RenderExec,Arguments++" "++ArgStr++" "++OutFile++" ", PortOpts, Handler)
    end.

section(F, Name) ->
    println(F, [io_lib:nl(),"<!-- Section ",Name," -->",io_lib:nl()]).

%%% write material code
write_default_materials(F)->
    println(F, "<material name=\"defaultMat\">"),
    println(F, "\t<type sval=\"shinydiffusemat\"/>"),
    println(F, "\t<color r=\"0.9\" g=\"0.9\" b=\"0.9\"/>"),
    println(F, "</material>\n"),
    %
    println(F, "<material name=\"w_blendone\">"),
    println(F, "\t<type sval=\"shinydiffusemat\"/>"),
    println(F, "\t<color r=\"0.789\" g=\"0.713\" b=\"0.794\"/>"),
    println(F, "</material>\n"),
    %
    println(F, "<material name=\"w_blendtwo\">"),
    println(F, "\t<type sval=\"glossy\"/>"),
    println(F, "\t<color r=\"1.0\" g=\"0.513\" b=\"0.594\"/>"),
    println(F, "</material>\n").

-include("thebounty/exp_material.erl").

%% write texture code
-include("thebounty/exp_texture.erl").

%% split material modulators
-include("thebounty/exp_modulators.erl").

% split geometry
-include("thebounty/exp_geometry.erl").

% split light code
-include("thebounty/exp_light.erl").

-include("thebounty/exp_camera.erl").

-include("thebounty/exp_world.erl").

-include("thebounty/exp_integrator.erl").

-include("thebounty/exp_render.erl").


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

%print(F, DeepString) ->
%    case file:write(F, DeepString) of
%        ok ->
%            ok;
%        Error ->
%            erlang:error(Error, [F,DeepString])
%    end.

println(F, DeepString) ->
    case file:write(F, [DeepString,io_lib:nl()]) of
        ok ->
            ok;
        Error ->
            erlang:error(Error, [F,DeepString])
    end.

%print(F, Format, Args) ->
%    case file:write(F, io_lib:format(Format, Args)) of
%        ok ->
%            ok;
%        Error ->
%            erlang:error(Error, [F,Format,Args])
%    end.

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

%!---------------------------------------
%! Convert certain terms to printable
%! strings in a hopefully efficient way.
%!---------------------------------------

export_rgb(F, Type, {R,G,B,_}) ->
    export_rgb(F, Type, {R,G,B});

export_rgb(F, Type, {R,G,B}) ->
    println(F, ["\t<",format(Type)," r=\"",format(R),"\" g=\"",format(G),"\" b=\"",format(B),"\"/>"]).

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

% new insert code from micheus ----------------------->
set_var(Name, Value) ->
    ets:insert(?LOCAL_MODULE, {Name,Value}).

get_var(Name) ->
    case ets:lookup(?LOCAL_MODULE, Name) of
        [] -> undefined;
        [{Name,Val}] -> Val
    end.

erase_var(Name) ->
    ets:delete(?LOCAL_MODULE, Name).

%%
% some useful declarations for User Interface


%%% pulls out all the values stored as {{KeyTag, SubKey}, Value}
%%% returns {ListOfFound, ListRemaining}
%%% ListOfFound is a list of {SubKey, Value}
rip_all(KeyTag, List) ->
    Keys = proplists:get_keys(List),
    rip_all(KeyTag, Keys, List).

rip_all(KeyTag, [Key | Keys], List) ->
    case rip_keytag(KeyTag, Key) of
    true ->
        {_SetTag, SubTag} = Key,
        Value = proplists:get_value(Key, List),
        ListNext = proplists:delete(Key, List),
        {Found, Remaining} = rip_all(KeyTag, Keys, ListNext),
        {[{SubTag, Value} | Found], Remaining};
    false ->
        rip_all(KeyTag, Keys, List)
    end;
rip_all(_K, _KL, List) ->
    {[], List}.

rip_keytag(KeyTag, {SetTag, _}) ->
    case KeyTag of
    SetTag -> true;
    _ -> false
    end;
rip_keytag(_KT, _ST) ->
    false.


%% Zip lists together into a list of tuples
%%
zip_lists([], []) -> [];
zip_lists([H1|T1], [H2|T2]) -> [{H1,H2}|zip_lists(T1, T2)].


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
-include("thebounty/ui_help.erl").
