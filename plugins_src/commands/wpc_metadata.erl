%%
%%  wpc_metadata.erl --
%%
%%     Metadata functionality
%%
%%  Copyright (c) 2026 Edward Blake
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wpc_metadata).
-export([init/0,menu/2,command/2]).

-include_lib("wings/src/wings.hrl").

init() ->
    true.

menu({tools},Menu) ->
    metadata_submenu(Menu);
menu({tools,metadata},Menu) ->
    metadata_menu_entry(Menu);
menu(_,Menu) -> Menu.

metadata_submenu([]) ->
    [{?__(1,"Metadata"), {metadata, []}}];
metadata_submenu([A|Menu]) ->
    [A|metadata_submenu(Menu)].

metadata_menu_entry([]) ->
    [{?__(1,"Scene"),scene,
      ?__(2,"Set scene metadata")},
     {?__(3,"Object"),object,
      ?__(4,"Set object metadata")}];
metadata_menu_entry([A|Menu]) ->
    [A|metadata_menu_entry(Menu)].

command({tools,{metadata,scene}},St) ->
    set_scene_metadata(St);
command({tools,{metadata,object}},St) ->
    set_obj_metadata(St);
command(_,_) ->
    next.

%%%
%%%

%%
%% Set Scene Metadata
%%

set_scene_metadata(#st{pst=Pst}=St) ->
    List = metadata_plugins(scene),
    QsFrames = metadata_tab_frames(scene,
        fun (Name) -> get_metadata(Name,Pst) end, List),
    Frame = [{oframe, QsFrames, 1, [{style, buttons}]}],
    wings_dialog:dialog(?__(1,"Scene Metadata"), {preview,Frame},
        fun
            ({dialog_preview,Args}) ->
                {preview,St,set_scene_metadata_1(Args, List, St)};
            (cancel) ->
                St;
            (Args) ->
                {commit,St,set_scene_metadata_1(Args, List, St)}
        end).

set_scene_metadata_1(Args, List, #st{pst=Pst}=St) ->
    Metadata = get_pst_metadata(Pst),
    Metadata_2 = lists:foldl(
        fun({Name, _, Mod}, Metadata_1) ->
            update_metadata(Name, Mod, scene, Args, Metadata_1)
        end, Metadata, List),
    St#st{pst=update_pst_metadata(Metadata_2, Pst)}.


%%
%% Object metadata.
%%
set_obj_metadata(#st{sel=[_|_],selmode=body}=St) ->
    List = metadata_plugins(object),
    QsFrames = metadata_tab_frames(object,
        fun (Name) -> get_obj_metadata(Name, St) end, List),
    Frame = [{oframe, QsFrames, 1, [{style, buttons}]}],
    wings_dialog:dialog(?__(1,"Object Metadata: ") ++ obj_names(St), {preview,Frame},
        fun
            ({dialog_preview,Args}) ->
                {preview,St,set_obj_metadata_1(Args, List, St)};
            (cancel) ->
                St;
            (Args) ->
                {commit,St,set_obj_metadata_1(Args, List, St)}
        end);
set_obj_metadata(#st{sel=[]}=_St) ->
    wings_u:error_msg(?__(3,"Need at least one object selected."));
set_obj_metadata(#st{selmode=SelMode}=St) when SelMode =/= body ->
    set_obj_metadata(wings_sel_conv:mode(body, St)).

set_obj_metadata_1(Args, List, St) ->
    wings_sel:map(fun (_, #we{pst=Pst}=We) ->
        Metadata = get_pst_metadata(Pst),
        Metadata_2 = lists:foldl(
            fun({Name, _, Mod}, Metadata_1) ->
                update_metadata(Name, Mod, object, Args, Metadata_1)
            end, Metadata, List),
        We#we{pst=update_pst_metadata(Metadata_2, Pst)}
    end, St).

obj_names(St) ->
    {Extra, StrList} =
        wings_sel:fold(
            fun
                (_, #we{name=ObjName}=_We, {0, Acc}) when length(Acc) < 3 ->
                    {0, [ObjName|Acc]};
                (_, #we{name=_}=_We, {C, Acc}) ->
                    {C + 1, Acc}
            end, {0, []}, St),
    lists:flatten(lists:join(?__(1,", "), lists:reverse(StrList)) ++
        if
            Extra =:= 1 -> io_lib:format(?__(2,", ~w more object"), [Extra]);
            Extra > 1   -> io_lib:format(?__(3,", ~w more objects"), [Extra]);
            true        -> ""
        end).

%%%
%%%

%% Find the metadata plugins
%%
metadata_plugins(Scope) ->
    Plugins = get(wings_plugins),
    lists:append([ try_metadata_plugin(Scope, Pl) || Pl <- Plugins])
        ++ [{comments, ?__(2,"Comments"), 0}].

%% Create Tab frames for dialog
%%
metadata_tab_frames(Scope, Fun, List) ->
    [{TabName, module_metadata_dialog(Mod, Name, Scope, Fun(Name))}
        || {Name, TabName, Mod} <- List].

%%%
%%%

update_metadata(Name, Mod, Scope, Args, Metadata) ->
    MetadataC = case proplists:get_value(Name, Metadata, []) of
        MetadataC_0 when is_list(MetadataC_0) -> MetadataC_0;
        _ -> []
    end,
    MetadataC_1 = module_metadata_update(Mod, Name, Scope, MetadataC, Args),
    case length(MetadataC_1) =:= 0 of
        true ->
            proplists:delete(Name, Metadata);
        false ->
            orddict:store(Name, MetadataC_1, orddict:from_list(proplists:delete(Name, Metadata)))
    end.

get_pst_metadata(Pst) ->
    case gb_trees:lookup(metadata,Pst) of
        none -> [];
        {value, Metadata} when is_list(Metadata) -> Metadata;
        {value, _} -> []
    end.

update_pst_metadata(Metadata, Pst) ->
    gb_trees:enter(metadata, Metadata, Pst).

get_metadata(Name, Pst) ->
    Metadata = get_pst_metadata(Pst),
    case proplists:get_value(Name, Metadata, []) of
        MetadataC when is_list(MetadataC) -> MetadataC;
        _ -> []
    end.


get_obj_metadata(Name, St) ->
    wings_sel:fold(fun (_, #we{pst=Pst}=_We, Acc) ->
        Metadata = get_pst_metadata(Pst),
        case proplists:get_value(Name,Metadata,Acc) of
            MetadataC when is_list(MetadataC) -> MetadataC;
            _ -> []
        end
    end, [], St).


module_metadata_dialog(0, comments, _Scope, MetadataC) ->
    metadata_comments_dialog(MetadataC);
module_metadata_dialog(Mod, Name, Scope, MetadataC) when is_atom(Mod) ->
    Mod:metadata_dialog(Scope, Name, MetadataC).

module_metadata_update(0, comments, _Scope, MetadataC, Args) ->
    metadata_comments_update(MetadataC, Args);
module_metadata_update(Mod, Name, Scope, MetadataC, Args) when is_atom(Mod) ->
    Mod:metadata_update(Scope, Name, MetadataC, Args).

try_metadata_plugin(Scope, Pl) ->
    try Pl:metadata_names(Scope) of
        Ret -> Ret
    catch
        error:_ -> []
    end.


%% Free-form comments
%%

metadata_comments_dialog(MetadataComments) when is_list(MetadataComments) ->
    Val = proplists:get_value(comments, MetadataComments, ""),
    {vframe,[
        {label,?__(12,"Comments")},
        {text,Val,[{key,comments},{width,50}]}
    ]}.

metadata_comments_update(MetadataComments, Args) when is_list(MetadataComments) ->
    case proplists:get_value(comments, Args, "") of
        "" ->
            proplists:delete(comments, MetadataComments);
        Creator ->
            orddict:store(comments, Creator, orddict:from_list(proplists:delete(comments, MetadataComments)))
    end.


