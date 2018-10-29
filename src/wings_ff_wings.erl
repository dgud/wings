%%
%%  wings_ff_wings.erl --
%%
%%     This module contain the functions for reading and writing .wings files.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id$
%%

-module(wings_ff_wings).
-export([import/2,merge/2,export/2]).

-include("wings.hrl").
-include_lib("wings/e3d/e3d_image.hrl").
-import(lists, [sort/1,reverse/1,foldl/3,any/2,keymember/3,keyfind/3]).

-define(WINGS_HEADER, "#!WINGS-1.0\r\n\032\04").

%% Load a wings file.

import(Name, St) ->
    wings_pb:start(?__(1,"opening wings file")),
    wings_pb:update(0.07, ?__(2,"reading file")),
    wings_pb:done(import_1(Name, false, St)).

merge(Name, St) ->
    wings_pb:start(?__(1,"opening wings file")),
    wings_pb:update(0.07, ?__(2,"reading file")),
    wings_pb:done(import_1(Name, true, St)).

import_1(Name, MrgDlg, St0) ->
    case file:read_file(Name) of
	{ok,<<?WINGS_HEADER,Sz:32,Data/binary>>} when byte_size(Data) =:= Sz ->
	    wings_pb:update(0.08, ?__(1,"converting binary")),
	    try binary_to_term(Data) of
		{wings,0,_Shapes} ->
                    {error, ?__(2,"Pre-0.80 Wings format no longer supported.")};
		{wings,1,_,_,_} ->
		    %% Pre-0.92. No longer supported.
                    {error,?__(3,"Pre-0.92 Wings format no longer supported.")};
		{wings,2,{Shapes,Materials,Props}} ->
		    Dir = filename:dirname(Name),
                    if MrgDlg -> import_vsn2_dlg(Shapes, Materials, Props, Dir, St0);
                       true -> ?SLOW(import_vsn2(Shapes, Materials, Props, Dir, St0))
                    end;
		{wings,_,_} ->
		    {error,?__(4,"unknown wings format")};
		Other ->
		    io:format("~P\n", [Other,20]),
                    {error,?__(5,"corrupt Wings file")}
	    catch
		error:badarg ->
                    {error,?__(5,"corrupt Wings file")}
	    end;
	{ok,_Bin} ->
	    {error,?__(6,"not a Wings file (or old Wings format)")};
	{error,Reason} ->
	    {error,file:format_error(Reason)}
    end.

-record(va, {color_lt=none,
	     color_rt=none,
	     uv_lt=none,
	     uv_rt=none}).

import_vsn2(Shapes, Materials0, Props, Dir, St0) ->
    wings_pb:update(0.10, ?__(1,"images and materials")),
    Images = import_images(Dir,Props),
    Materials1 = translate_materials(Materials0),
    Materials2 = translate_map_images(Materials1, Images),
    Materials = translate_object_modes(Materials2, Shapes),
    {St1,NameMap0} = wings_material:add_materials(Materials, St0),
    NameMap1 = gb_trees:from_orddict(sort(NameMap0)),
    NameMap = optimize_name_map(Materials, NameMap1, []),
    St = import_props(Props, St1),
    wings_pb:update(1.0,?__(2,"objects")),
    import_objects(Shapes, NameMap, St).

import_vsn2_dlg(Shapes0, Materials0, Props5, Dir,
                #st{selmode=Mode0,sel=Sel0,shapes=Shps0,mat=Mat0,ssels=Ssels0,views={_,Views0}}=St0) ->
    %% Current elements names
    {OldShpNames,OldLgtNames} = lists:foldr(
                                fun(#we{name=Name}=We, {OAcc,LAcc}) when ?IS_LIGHT(We) ->
                                        {OAcc, LAcc++[Name]};
                                   (#we{name=Name}, {OAcc,LAcc}) ->
                                        {OAcc++[Name], LAcc}
                                end, {[],[]}, gb_trees:values(Shps0)),
    OldMtlNames = gb_trees:keys(Mat0),
    OldVwsNames = [Name || {_,Name} <- tuple_to_list(Views0)],
    OldSelGroups = [Id || {{_Mode,_Name}=Id,_} <- gb_trees:to_list(Ssels0)],

    %% New elements names
    ObjInfo = [{Name, case import_perm(Props) of
                           0 -> {0,0};
                           1 -> {1,0}; % locked
                           2 -> {0,1}; % hidden
                           3 -> {1,1}; % hidden_locked
                           {_,_} -> {1,0} % hidden
                       end} || {object,Name,_,Props} <- Shapes0],

    ShpNames = [Name || {Name,_} <- ObjInfo],
    {Locked,Hidden} = lists:foldr(fun({_, {L, H}}, {LAcc, HAcc}) ->
                                    {LAcc+L, HAcc+H}
                                end, {0,0}, ObjInfo),

    %% getting Lights name
    PrpLights = proplists:get_value(lights,Props5,[]),
    LgtNames = [Name || {Name,_} <- PrpLights],

    %% getting Materials name
    MtlNames = [Name || {Name,_} <- Materials0]--[default],

    %% getting Views name
    PrpViews1 = proplists:get_value(views,Props5,[]),
    VwsNames0 = [proplists:get_value(name,Fields) || {view, Fields} <- PrpViews1],
    VwsNames = VwsNames0 --["current_view"],

    %% getting Images name
    PrpImages = proplists:get_value(images,Props5,[]),
    ImgNames = lists:foldr(fun({_Id,ImgProps}, Acc)->
                                 Status=case proplists:get_value(filename, ImgProps) of
                                            undefined -> "";
                                            _ -> " ("++?__(4,"External")++")"
                                        end,
                                 Acc ++[proplists:get_value(name,ImgProps)++Status]
                         end, [], PrpImages),

    %% getting Selection Groups names and ID
    PrpSelGrp = get_sel_groups(Props5,[]),
    SelGrNames = [{{{Mode,Name},Name ++" (" ++ atom_to_list(Mode) ++ ")"}} || {{_,Name},{Mode,_}} <- PrpSelGrp],

    %% getting Palettes
    PrpPalette = proplists:get_value(palette,Props5,[]),
    %% getting Selections
    PrpSel = proplists:get_value(selection,Props5,[]),
    %% getting Scene options for render plugins
    PrpScenes = proplists:get_value(scene_prefs,Props5,[]),

    %% preparing the dialog
    Qs = make_merge_dlg(ShpNames,LgtNames,MtlNames,ImgNames,SelGrNames,VwsNames,
                        PrpPalette=/=[],PrpSel=/=[],PrpScenes=/=[],Locked=/=0,Hidden=/=0),
    Fun=fun(Result) ->
                wings_pb:start(?__(1,"processing merge options...")),
                MrgObj=proplists:get_value({objects,mrg},Result),    % all/selected/none
                MrgLgt=proplists:get_value({lights,mrg},Result),     % all/selected/none
                MrgMtl=proplists:get_value({materials,mrg},Result),  % all/used/selected/none
                MrgImg=proplists:get_value({images,mrg},Result),     % all/used/none
                MrgSgr=proplists:get_value({selgroups,mrg},Result),  % used/none
                MrgVws=proplists:get_value({views,mrg},Result),      % all/selected/none
                MrgPlt=proplists:get_value(palette,Result),          % false/true
                MrgScn=proplists:get_value(scene_pref,Result),       % false/true
                NewSel=proplists:get_value(new_sel,Result),          % false/true
                ObjOpt=proplists:get_value({objects,opt},Result),    % keep/delete
                LgtOpt=proplists:get_value({lights,opt},Result),     % keep/delete
                MtlOpt=proplists:get_value({materials,opt},Result),  % keep/delete
                VwsOpt=proplists:get_value({views,opt},Result),      % keep/delete
                SelOpt=proplists:get_value(sel_opt,Result),          % keep/add/replace
                Unlock=proplists:get_value(unlock,Result),           % false/true
                Unhide=proplists:get_value(unhide,Result),           % false/true

                {Shps,Objs1,Lgts0} =
                    case {MrgObj,MrgLgt} of
                        {none,none} -> {Shps0,[],[]};  % there are no object and light to be processed
                        _ ->
                            {Shapes2,ShpChkNames} = prepare_items_list(objects,Result,Shapes0,OldShpNames),
                            {Lights2,LgtChkNames} = prepare_items_list(lights,Result,PrpLights,OldLgtNames),

                            Shapes3 = case ObjOpt of
                                        none -> [];
                                        _ -> Shapes2
                                    end,

                            {_,Shps1,Objs0,_,Lgts1,_} =
                                lists:foldl(fun process_obj_light/2,
                                            {{{MrgLgt,LgtOpt},{MrgObj,ObjOpt}},[],Shapes3,ShpChkNames,Lights2,LgtChkNames},
                                            gb_trees:to_list(Shps0)),
                            {gb_trees:from_orddict(Shps1),Objs0,Lgts1}
                    end,

                %% processing materials lists in accord with the user choice
                Materials1 = if MrgMtl=:=used -> used_material_list(Materials0,Objs1,Lgts0);
                              true -> Materials0
                           end,
                {Materials2,MtlChkNames0} = prepare_items_list(materials,Result,Materials1,OldMtlNames),
                MtlChkNames = [atom_to_list(I) || I <- (MtlChkNames0--[default])],  % material uses atom names
                {_,Mtls0,NMtls0,_,RMtls} =
                    lists:foldl(fun process_material/2,
                                {{MrgMtl,MtlOpt},[],Materials2,MtlChkNames,[]},
                                gb_trees:to_list(Mat0)),

                %% removing images references from materials if none is going to be imported
                NMtls = if MrgImg =:= none ->
                              lists:foldl(fun({Name,Props0}, Acc) ->
                                                  Props = case proplists:get_value(maps,Props0,[]) of
                                                            [] -> Props0;
                                                            _ -> proplists:delete(maps,Props0)++[{maps,[]}]
                                                        end,
                                                  Acc++[{Name,Props}]
                                          end, [], NMtls0);
                         true -> NMtls0
                      end,

                Mtls = gb_trees:from_orddict(Mtls0),

                Objs = replace_materials(RMtls,Unlock,Unhide,Objs1),

                Lgts = replace_light_visibility(Lgts0,Unlock,Unhide),

                Props4 = proplists:delete(lights, Props5)++[{lights,Lgts}],

                %% processing Images lists in accord with the user choice
                Props3 =
                    case MrgImg of
                        used ->
                            UsedImg0=
                                lists:foldl(fun({_,Props}, Acc) ->
                                                    case proplists:get_value(maps,Props,[]) of
                                                        [] -> Acc;
                                                        Map -> lists:foldl(fun({_Type,Id}, Acc0) ->
                                                                                   gb_sets:add(Id,Acc0)
                                                                           end, Acc, Map)
                                                    end
                                            end, gb_sets:new(), NMtls),
                            Images=
                                gb_sets:fold(fun(Id, Acc) ->
                                                     Acc++[{Id,proplists:get_value(Id,PrpImages)}]
                                             end, [], UsedImg0),
                            proplists:delete(images, Props4)++[{images,Images}];
                        _ ->
                            merge_prop({images,MrgImg}, Props4)
                    end,

                %% processing Views lists in accord with the user choice
                Props2 =
                    if MrgVws=/=none ->
                        %% we need to remove the current_view from the elements list here,
                        %% since it was not present in the names table for selection
                        PrpViews =
                            case find_view_name("current_view",PrpViews1,1) of
                                {Idx0,_} ->
                                    lists:delete(lists:nth(Idx0,PrpViews1),PrpViews1);
                                _ -> PrpViews1
                            end,

                        {Views,VwsChkNames}=prepare_items_list(views,Result,PrpViews,OldVwsNames),
                        {_,Vws0,NVws,_,_RVws}=
                            lists:foldl(fun process_view/2,
                                        {{MrgVws,VwsOpt},[],Views,VwsChkNames,[]}, tuple_to_list(Views0)),
                        Vws = list_to_tuple(Vws0),
                        proplists:delete(views,Props3)++[{views,NVws}];
                    true ->
                        Vws = Views0,
                        proplists:delete(views,Props3)
                    end,

                %% processing Selection Groups lists in accord with the user choice
                Props1 =
                    if MrgSgr=/=none ->
                            PrpSelGrp0 = used_selgroup_list(Result,PrpSelGrp,Shapes0),
                            {SelGrp,SgrChkNames}=prepare_items_list(selgroups,Result,PrpSelGrp0,OldSelGroups),
                            {_,_,NSgr,_,_RSgr}=
                                lists:foldr(fun process_selgroup/2,
                                            {MrgSgr,[],SelGrp,SgrChkNames,[]},SelGrp),
                            remove_selgroups(Props2)++NSgr;
                        true ->
                            remove_selgroups(Props2)
                    end,

                Props0 =
                    if MrgPlt=:=false ->
                            proplists:delete(palette,Props1);
                        true -> Props1
                    end,
                Props =
                    if MrgScn=:=false ->
                            proplists:delete(scene_prefs,Props0);
                        true -> Props0
                    end,

                wings_pb:update(0.8, ?__(3,"converting binary")),
                #st{shapes=NewShps} = St2 =
                    wings_pb:done(import_vsn2(Objs, NMtls, Props, Dir, St0#st{shapes=Shps,mat=Mtls,views={tuple_size(Vws),Vws}})),
                St1 =
                    if (SelOpt=/=keep) and NewSel ->
                            SelShp = gb_trees:to_list(NewShps) -- gb_trees:to_list(Shps0),
                            case SelShp of
                                [] -> St2;
                                _ ->
                                    #st{sel=SelTmp} = wings_sel:make(fun(_,#we{}) -> true
                                                                   end,body,St2#st{shapes=gb_trees:from_orddict(SelShp)}),
                                    St2#st{selmode=body,sel=SelTmp}
                            end;
                       true -> St2
                    end,
                St = wings_sel:valid_sel(merge_sel(SelOpt,Mode0,Sel0,St1)),
                wings_obj:recreate_folder_system(St#st{saved=false})
        end,
    wings_dialog:dialog("Merge", Qs, ?SLOW(Fun)),
    St0.

get_sel_groups([], Acc) -> Acc;
get_sel_groups([{{selection_group,_},_}=H|T], Acc) -> get_sel_groups(T,Acc++[H]);
get_sel_groups([_|T], Acc) -> get_sel_groups(T,Acc).

remove_selgroups(List) ->
    remove_selgroups_0(List, []).
remove_selgroups_0([], Acc) -> Acc;
remove_selgroups_0([{{selection_group,_},_}|T], Acc) ->
    remove_selgroups_0(T,Acc);
remove_selgroups_0([H|T], Acc) ->
    remove_selgroups_0(T,Acc++[H]).

process_obj_light({_Id,#we{name=Name}=We}=I, {{{MrgLgt,LgtOpt},_}=Opt, AShp, AObj, ASNames, ALgt, ALNames}) when ?IS_LIGHT(We) ->
    case MrgLgt of
        none ->
            {Opt,AShp++[I],AObj,ASNames,ALgt,ALNames};
        _ ->
            LgtRst=lists:keyfind(Name,1,ALgt),
            case LgtRst of
                {Name,F0} ->
                    if LgtOpt=:=keep ->  % Keep if existent object (same name)
                            Name0 = wings_util:unique_name(Name, ALNames),
                            ALgt0 = lists:keydelete(Name,1,ALgt),
                            {Opt,AShp++[I],AObj,ASNames,ALgt0++[{Name0,F0}],ALNames++[Name0]};
                        true ->  % Delete an existent object
                            {Opt,AShp,AObj,ASNames,ALgt,ALNames}
                    end;
                _ -> {Opt,AShp++[I],AObj,ASNames,ALgt,ALNames}
            end
    end;
process_obj_light({_Id,#we{name=Name}}=I, {{_,{MrgObj,ObjOpt}}=Opt, AShp, AObj, ASNames, ALgt, ALNames}) ->
    case MrgObj of
        none ->
            {Opt,AShp++[I],AObj,ASNames,ALgt,ALNames};
        _ ->
            ObjRst = lists:keyfind(Name,2,AObj),
            case ObjRst of
                {object,Name,F0,F1} ->
                    if ObjOpt=:=keep ->
                            Name0 = wings_util:unique_name(Name, ASNames),
                            AObj0 = lists:keydelete(Name,2,AObj),
                            {Opt,AShp++[I],AObj0++[{object,Name0,F0,F1}],ASNames++[Name0],ALgt,ALNames};
                        true ->
                            {Opt,AShp,AObj,ASNames,ALgt,ALNames}
                    end;
                _ ->
                    {Opt,AShp++[I],AObj,ASNames,ALgt,ALNames}
            end
    end.

process_material({Name,_}=I, {{MrgMtl,MtlOpt}=Opt, AOld, ANew, AMNames, ARplNames}) ->
        case MrgMtl of
            none ->
                {Opt,AOld++[I],ANew,AMNames,ARplNames};
            _ ->
                MtlRst = lists:keyfind(Name,1,ANew),
                case MtlRst of
                    {Name,Props0} ->
                        if MtlOpt=:=keep ->  % Keep if existent material (same name)
                                Name0 = list_to_atom(wings_util:unique_name(atom_to_list(Name), AMNames)),
                                ANew0 = lists:keydelete(Name,1,ANew),
                                {Opt,AOld++[I],ANew0++[{Name0,Props0}],AMNames++[Name0],ARplNames++[{Name,Name0}]};
                            true ->  % Delete an existent material
                                {Opt,AOld,ANew,AMNames,ARplNames}
                        end;
                    _ ->
                        {Opt,AOld++[I],ANew,AMNames,ARplNames}
                end
        end.

process_view({_,Name}=I, {{MrgVws,VwsOpt}=Opt, AOld, ANew, AMNames, ARplNames}) ->
    case MrgVws of
        none ->
            {Opt,AOld++[I],ANew,AMNames,ARplNames};
        _ ->
            case find_view_name(Name,ANew,1) of
                {Idx,{view,Props0}} ->
                    if VwsOpt=:=keep ->  % Keep if existent view (same name)
                            Name0 = wings_util:unique_name(Name, AMNames),
                            ANew0 = lists:delete(lists:nth(Idx,ANew),ANew),
                            Props = [{name,Name0}]++proplists:delete(name,Props0),
                            {Opt, AOld++[I],ANew0++[{view,Props}],AMNames++[Name0],ARplNames++[{Name,Name0}]};
                        true ->  % Delete an existent selection
                            {Opt,AOld,ANew,AMNames,ARplNames}
                    end;
                _ ->
                    {Opt,AOld++[I],ANew,AMNames,ARplNames}
            end
    end.

process_selgroup({Key,{Mode,_}}=I, {MrgSgr, AOld, ANew, AMNames, ARplNames}) ->
    case MrgSgr of
        none ->
            {MrgSgr,AOld++[I],ANew,AMNames,ARplNames};
        _ ->
            %% Selection Groups can have duplicate names (Key), but not for the same selection mode
            %% we need to check all new selection for duplicate
            SgrRst = lists:foldr(fun(Item,Res)->
                                    case Item of
                                        {Key,{Mode,_}} -> Item;
                                        _ -> Res
                                    end
                                end, undefined, ANew),
            case SgrRst of
                {{_,Name}=Key,Props0} ->
                    AMNames0=[Name0 || {Mode0,Name0} <- AMNames, Mode0=:=Mode],
                    Name0 = wings_util:unique_name(Name, AMNames0),
                    ANew0 = lists:delete(SgrRst,ANew),
                    {MrgSgr,AOld++[I],ANew0++[{{selection_group,Name0},Props0}],AMNames++[{Mode,Name0}],ARplNames++[{{Mode,Name},{Mode,Name0}}]};
                _ ->
                    {MrgSgr,AOld++[I],ANew,AMNames,ARplNames}
            end
    end.

find_view_name(_, [], _) -> undefined;
find_view_name(Name, [{view,Props}=H|T], Idx) ->
    {_,Name0} = lists:keyfind(name,1,Props),
    if  Name0 =:= Name -> {Idx,H};
        true -> find_view_name(Name,T, Idx+1)
    end.

replace_materials([], Unlock, Unhide, Shs) when Unlock=:=true; Unhide=:=true ->
    [replace_materials_0({Unlock,Unhide},Sh0) || Sh0 <- Shs];
replace_materials([], _, _, Shs) -> Shs;
replace_materials(Names, Unlock, Unhide, Shs) ->
    [replace_materials_0(Names,{Unlock,Unhide},Sh0) || Sh0 <- Shs].

replace_materials_0(NewPerm, {object,Name,Winged, Props0}) ->
    Props=replace_perm(NewPerm, Props0),
    {object,Name,Winged,Props}.
replace_materials_0(Names, NewPerm, {object,Name,{winged,Es,Fs0,Vs,He}, Props0}) ->
    Fs=lists:reverse(replace_materials_1(Fs0,Names,[])),
    Props=replace_perm(NewPerm, Props0),
    {object,Name,{winged,Es,Fs,Vs,He},Props}.

replace_materials_1([], _, Acc) -> Acc;
replace_materials_1([[{material,Name}]=H|T], Names, Acc) ->
    Fs=case lists:keyfind(Name,1,Names) of
           {Name,NewName} -> [{material,NewName}];
           _ -> H
       end,
    replace_materials_1(T, Names, [Fs|Acc]);
replace_materials_1([H|T], Names, Acc) ->
    replace_materials_1(T, Names, [H|Acc]).

replace_perm({Unlock,Unhide}, Props) ->
    State=proplists:get_value(state, Props),
    case replace_perm_0(State,Unlock,Unhide) of
        State -> Props;
        ignore -> Props;
        undefined -> proplists:delete(state, Props);
        NewPerm -> proplists:delete(state, Props)++[{state,NewPerm}]
    end.

replace_perm_0(locked, true=_Unlock, _Unhide) -> undefined;
replace_perm_0(locked, _, _) -> locked;
replace_perm_0(hidden, _, true) -> undefined;
replace_perm_0(hidden, _, _) -> hidden;
replace_perm_0({hidden,_,_}, _, true) -> undefined;
replace_perm_0(hidden_locked, true, true) -> undefined;
replace_perm_0(hidden_locked, true, false) -> hidden;
replace_perm_0(hidden_locked, false, true) -> locked;
replace_perm_0(_, _, _) -> ignore.

replace_light_visibility(Lgts, Unlock, Unhide) ->
    [{Name, replace_light_perm(Props,Unlock,Unhide)} || {Name,Props} <- Lgts].

replace_light_perm(Props1,Unlock,Unhide) ->
    Props0 =
        if Unhide=:=true -> lists:keyreplace(visible,1,Props1,{visible,true});
            true -> Props1
        end,
    if Unlock=:=true -> lists:keyreplace(locked,1,Props0,{locked,false});
        true -> Props0
    end.

prepare_items_list(Key, Result, Elements, OldElmNames) ->
    MrgOpt = proplists:get_value({Key,mrg},Result),
    case get_table_names({Key,table},Result) of
        {Sel, ElmNames0} ->  % Data contains values (text or atom) relative to the text list
            case MrgOpt of
                Op when Op=:=all; Op=:=used ->
                    {Elements,merge_names(OldElmNames,ElmNames0)};
                selected ->
                    case Sel of
                        [] ->
                            {[],[]}; % user can has choose Selected, but didn't select an item
                        _ ->
                            {SelItems,SelNames} =
                                lists:foldl(fun(Idx, {IAcc,NAcc})->
                                                    {IAcc++[lists:nth(Idx+1,Elements)],
                                                     NAcc++[lists:nth(Idx+1,ElmNames0)]}
                                            end,{[],[]},Sel),
                            {SelItems,merge_names(OldElmNames,SelNames)}
                    end;
                none ->
                    {[],[]}
            end;
        _ ->  {[],[]}
    end.

used_material_list(_,[]=_Objs,_) -> [];
used_material_list(Materials0, Objs,_Lgts) ->
    Fs0=[Fs || {object,_,{winged,_,Fs,_,_},_} <- Objs],
    UsedMtl=gb_sets:from_list(lists:flatten(Fs0)),
    Materials=gb_sets:fold(fun({material,Name}, Acc) ->
                                   case proplists:lookup(Name,Materials0) of
                                       none -> Acc;
                                       M -> Acc++[M]
                                   end
                           end, [], UsedMtl),
    Materials.

used_selgroup_list(_,_,[]=_Shapes) -> [];
used_selgroup_list(Result,SelGroup, Shapes) ->
    MrgOpt = proplists:get_value({objects,mrg},Result),
    if MrgOpt=/=none ->
            SelObjs =
                case get_table_names({objects,table},Result) of
                    {Sel, ElmNames} ->
                        case Sel of
                            [] ->
                                {_, Res} = lists:foldr(fun(Item, {Id,Acc}) ->
                                    {Id,Acc++[{Id,element(2,Item)}]}
                                end,{0,[]},Shapes),
                                Res;
                            _ ->
                                [{Idx,lists:nth(Idx+1,ElmNames)} || Idx <- Sel] % Data contains values (text or atom) relative to the text list
                        end;
                    _ -> []
                end,
            lists:foldl(fun({{selection_group,_}=Key,{Mode,Sel0}}, Acc) ->
                            Sel2 = lists:foldr(fun({Id,_}=Sel1, Acc0) ->
                                                    case lists:keymember(Id, 1, SelObjs) of
                                                        true -> Acc0++[Sel1];
                                                        _ -> Acc0
                                                    end
                                                 end, [], Sel0),
                            if Sel2=/=[] ->
                                    Acc++[{Key,{Mode,Sel2}}];
                                true -> Acc
                            end
                        end,[],SelGroup);
        true -> []
    end.

merge_sel(replace, _, _, St) -> St;
merge_sel(keep, SrcMode, SrcSel, St) ->
    St#st{selmode=SrcMode,sel=SrcSel};
merge_sel(add, SrcMode, SrcSel, #st{selmode=MrgMode}=St0) ->
    St=if SrcMode=/=MrgMode -> wings_sel_conv:mode(SrcMode,St0);
          true -> St0
       end,
    #st{sel=MrgSel}=St,
    Sel=gb_sets:union(gb_sets:from_list(MrgSel), gb_sets:from_list(SrcSel)),
    St#st{sel=gb_sets:to_list(Sel)}.

merge_prop({Key, Import}, Props) ->
    case Import of
        none -> proplists:delete(Key, Props);
        _ -> Props
    end.

%% returns a list without duplicated names
merge_names(LstNames0, LstNames1) ->
    NSet0=sets:from_list(LstNames0),
    NSet1=sets:from_list(LstNames1),
    sets:to_list(sets:union(NSet0,NSet1)).

%% returns the selected element idx and the list of elements names
get_table_names(Key, List0)->
    List=[I || I <- List0, is_tuple(I)],
    case proplists:get_value(Key,List,undefined) of
        {Sel,Rows} -> {Sel, [Name || {{Name,_Label}} <-Rows]};
        _ -> undefined
    end.

make_merge_dlg(Obj, Lgt, Mtl, Img, SGr, Vws, Plt, Sel, Scn, Locked, Hidden) ->
    [
        {vframe, [
            {vframe,[
                {oframe,
                        make_dlg_item(?__(1,"Objects"),Obj,Obj=/=[],objects) ++
                        make_dlg_item(?__(2,"Lights"),Lgt,Lgt=/=[],lights) ++
                        make_dlg_item(?__(3,"Materials"),Mtl,Mtl=/=[],materials) ++
                        make_dlg_item(?__(4,"Images"),Img,Img=/=[],images) ++
                        make_dlg_item(?__(5,"Selection Groups"),SGr,SGr=/=[],selgroups) ++
                        make_dlg_item(?__(6,"Saved Views"),Vws,Vws=/=[],views),
                    1, [{style, buttons}]},
                {hframe,[
                    {vframe, [
                        {?__(7,"Unlock locked elements"),false, [{key, unlock},{hook,dlg_hook_enable(Locked)}]},
                        {?__(9,"Merge Palette"),Plt, [{key, palette},{hook,dlg_hook_enable(Plt)}]}
                    ]},
                    {vframe, [
                        {?__(8,"Unhide hidden elements"),false, [{key, unhide},{hook,dlg_hook_enable(Hidden)}]},
                        {?__(17,"Merge render settings"),Scn, [{key, scene_pref},{hook,dlg_hook_enable(Scn)}]}
                    ]}
                ]}
            ],[{margin,false}]},
            {vframe,[
                {?__(15,"Make all new Selection"),false,[{key, new_sel},{hook,dlg_hook_enable(true)},
                    {info, ?__(16,"It makes all new elements selected in accord with the options bellow")}]
                },
                {hradio, [
                    {?__(10,"Ignore"),keep},
                    {?__(11,"Add to current"),add},
                    {?__(12,"Replace the current"),replace}
                ], keep, [{key,sel_opt},{hook,dlg_hook_enable(Sel)},
                    {info, ?__(14,"Defines what to do with selections present in the file being merged")}]}
            ],[{title,?__(13,"Selection options")},{margin,false}]}
        ]}
    ].

make_dlg_item(Title, List, Enabled, Key) ->
    Rows=
        case Key of
            Key when Key =:= selgroups -> List;
            _ ->
                [{{Name, if is_atom(Name) -> atom_to_list(Name);
                             true -> Name
                         end}} || Name <- List]
        end,
    Fields0=[
        {table,[{?__(1,"Element name")}|Rows],[{key,{Key,table}},{max_rows,10},{col_widths,{40}},{hook,fun dlg_hook_select/3}]},
        make_dlg_opt_mrg0(Enabled, Key)],
    Fields1=make_dlg_opt_mrg1(Enabled, Key),
    [{Title ++io_lib:format(" (~p)",[length(List)]), {vframe, Fields0++Fields1}}].

make_dlg_opt_mrg0(Enabled, Key) ->
    {hframe, [
        {hradio,
            make_dlg_opt_mrg0_0(Key),
            merge_opt_default(Enabled,Key),[{key,{Key,mrg}},{title,?__(1,"What to merge")},
            {hook,dlg_hook_enable(Enabled)},
            {info,?__(2,"Define what element(s) to merge into the current scene")}]}
    ],[{margin,false}]}.

make_dlg_opt_mrg0_0(images) ->
    [{?__(1,"All")++" ",all},
     {?__(2,"Only used by any material")++" ",used},
     {?__(3,"None")++" ",none}];
make_dlg_opt_mrg0_0(materials) ->
    [{?__(1,"All")++" ",all},
     {?__(4,"Selected")++" ",selected},
     {?__(5,"Used by chosen objects")++" ",used},
     {?__(3,"None")++" ",none}];
make_dlg_opt_mrg0_0(selgroups) ->
    [{?__(5,"Used by chosen objects")++" ",used},
     {?__(3,"None")++" ",none}];
make_dlg_opt_mrg0_0(_) ->
    [{?__(1,"All")++" ",all},
     {?__(4,"Selected")++" ",selected},
     {?__(3,"None")++" ",none}].

make_dlg_opt_mrg1(_, Key) when Key=:=images; Key=:=selgroups -> [];
make_dlg_opt_mrg1(Enabled, Key) ->
    [{hframe, [
        {hradio, [
            {?__(3,"Rename new"),keep},
            {?__(4,"Replace current"),delete}
        ],keep,[{key,{Key,opt}}, {title,?__(1,"Same name action")},
            {hook,dlg_hook_enable(Enabled)},
            {info,?__(2,"Define what to do if there is an other element with identical name in the current scene")}]}
    ],[{margin,false}]}].

dlg_hook_select({Item,_}, _, _) when Item=:=images; Item=:=selgroups -> ok;
dlg_hook_select({Item,_}=_Key, _Ctrl, Store) ->
    wings_dialog:set_value({Item,mrg}, selected, Store).

dlg_hook_enable(Enabled) ->
    fun(Key, Value, Store)->
            case Key of
                {objects,opt} ->
                    Value0 =
                        case wings_dialog:get_value({lights,opt}, Store) of
                            [] -> false;
                            Val -> Val =/= none
                        end,
                    wings_dialog:enable(sel_opt, (Value =/= none) or Value0 , Store);
                {lights,opt} ->
                    Value0 =
                        case wings_dialog:get_value({objects,opt}, Store) of
                            [] -> false;
                            Val -> Val =/= none
                        end,
                    wings_dialog:enable(sel_opt, (Value =/= none) or Value0 , Store);
                new_sel ->
                    Value0 = if Value =:= true -> add;
                                 true -> keep
                             end,
                    wings_dialog:set_value(sel_opt, Value0, Store);
                sel_opt ->
                    if Value =:= keep ->
                            wings_dialog:set_value(new_sel, false, Store);
                        true -> ok
                    end;
                _ ->
                    wings_dialog:enable(Key, Enabled, Store)
            end
    end.

merge_opt_default(false, _) -> none;
merge_opt_default(_, Key) when Key =:= images; Key =:= materials; Key=:=selgroups -> used;
merge_opt_default(_, _) -> all.

optimize_name_map([{Name,_}|Ms], NameMap, Acc) ->
    case gb_trees:lookup(Name, NameMap) of
	none ->
	    optimize_name_map(Ms, NameMap, [{Name,Name}|Acc]);
	{value,NewName} ->
	    optimize_name_map(Ms, NameMap, [{Name,NewName}|Acc])
    end;
optimize_name_map([], _, Acc) -> gb_trees:from_orddict(sort(Acc)).

import_objects(Shapes, NameMap, #st{selmode=Mode,shapes=Shs0,onext=Oid0}=St) ->
    {Objs,Oid} = import_objects(Shapes, Mode, NameMap, Oid0, []),
    Shs = gb_trees:from_orddict(gb_trees:to_list(Shs0) ++ Objs),
    St#st{shapes=Shs,onext=Oid}.

import_objects([Sh0|Shs], Mode, NameMap, Oid, ShAcc) ->
    {object,Name,{winged,Es,Fs,Vs,He},Props} = Sh0,
    Etab = import_edges(Es, 0, []),
    %% The 'default' material saved in this .wings file might not
    %% match the current default material, so it could have been
    %% renamed (to 'default2', for instance). We must make sure
    %% that we use the correctly named default material on faces
    %% without explicit material.
    DefaultMat = case gb_trees:lookup(default, NameMap) of
		     none -> default;
		     {value,DefaultMat0} -> DefaultMat0
		 end,
    FaceMat = import_face_mat(Fs, NameMap, DefaultMat, 0, []),
    Vtab = import_vs(Vs, 0, []),
    Htab = gb_sets:from_list(He),
    Perm = import_perm(Props),
    Mirror = proplists:get_value(mirror_face, Props, none),
    Holes = proplists:get_value(holes, Props, []),
    Pst0 = proplists:get_value(plugin_states, Props, []),
    Pst = try gb_trees:from_orddict(Pst0)
	  catch error:_ -> gb_trees:empty()
	  end,
    We = #we{he=Htab,perm=Perm,holes=Holes,pst=Pst,
	     id=Oid,name=Name,mirror=Mirror,mat=FaceMat},
    HiddenFaces = proplists:get_value(num_hidden_faces, Props, 0),
    import_objects(Shs, Mode, NameMap, Oid+1, [{HiddenFaces,We,{Vtab,Etab}}|ShAcc]);
import_objects([], _Mode, _NameMap, Oid, Objs0) ->
    %%io:format("flat_size: ~p\n", [erts_debug:flat_size(Objs0)]),
    Objs = share_list(Objs0),
    %%io:format("size: ~p\n", [erts_debug:size(Objs)]),
    {Objs,Oid}.
    
import_edges([[{edge,Va,Vb,Lf,Rf,Ltpr,Ltsu,Rtpr,Rtsu}]|Es], Edge, Acc) ->
    Rec = #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf,
		ltpr=Ltpr,ltsu=Ltsu,rtpr=Rtpr,rtsu=Rtsu},
    EdgeData = {Rec,none},
    import_edges(Es, Edge+1, [{Edge,EdgeData}|Acc]);
import_edges([E|Es], Edge, Acc) ->
    EdgeData = import_edge(E, none, #va{}),
    import_edges(Es, Edge+1, [{Edge,EdgeData}|Acc]);
import_edges([], _Edge, Acc) -> reverse(Acc).

import_edge([{edge,Va,Vb,Lf,Rf,Ltpr,Ltsu,Rtpr,Rtsu}|T], _, Attrs) ->
    Rec = #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf,
		ltpr=Ltpr,ltsu=Ltsu,rtpr=Rtpr,rtsu=Rtsu},
    import_edge(T, Rec, Attrs);
import_edge([{uv_lt,<<U/float,V/float>>}|T], Rec, Attrs) ->
    import_edge(T, Rec, Attrs#va{uv_lt={U,V}});
import_edge([{uv_rt,<<U/float,V/float>>}|T], Rec, Attrs) ->
    import_edge(T, Rec, Attrs#va{uv_rt={U,V}});
import_edge([{color_lt,<<R:32/float,G:32/float,B:32/float>>}|T], Rec, Attrs) ->
    import_edge(T, Rec, Attrs#va{color_lt={R,G,B}});
import_edge([{color_rt,<<R:32/float,G:32/float,B:32/float>>}|T], Rec, Attrs) ->
    import_edge(T, Rec, Attrs#va{color_rt={R,G,B}});
import_edge([{color,Bin}|T], Rec, Attrs) ->
    %% Old-style vertex colors (pre 0.98.15).
    <<R1/float,G1/float,B1/float,R2/float,G2/float,B2/float>> = Bin,
    import_edge(T, Rec, Attrs#va{color_lt={R1,G1,B1},color_rt={R2,G2,B2}});
import_edge([{uv,Bin}|T], Rec, Attrs) ->
    %% Old-style UV coordinates (pre 0.98.15).
    <<U1/float,V1/float,U2/float,V2/float>> = Bin,
    import_edge(T, Rec, Attrs#va{uv_lt={U1,V1},uv_rt={U2,V2}});
import_edge([_|T], Rec, Attrs) ->
    import_edge(T, Rec, Attrs);
import_edge([], Rec, Attrs) -> {Rec,Attrs}.

import_face_mat([F|Fs], NameMap, Default, Face, Acc) ->
    Mat = import_face_mat_1(F, NameMap, Default),
    import_face_mat(Fs, NameMap, Default, Face+1, [{Face,Mat}|Acc]);
import_face_mat([], _, _, _, Acc) -> reverse(Acc).

import_face_mat_1([{material,Name}|T], NameMap, Default) ->
    %% Silently ignore materials not found in the name map.
    Mat = case gb_trees:lookup(Name, NameMap) of
	      none -> Default;
	      {value,Other} -> Other
	  end,
    import_face_mat_1(T, NameMap, Mat);
import_face_mat_1([_|T], NameMap, Mat) ->
    import_face_mat_1(T, NameMap, Mat);
import_face_mat_1([], _, Mat) -> Mat.

import_vs([Vtx|Vs], V, Acc) -> 
    Rec = import_vertex(Vtx, []),
    import_vs(Vs, V+1, [{V,Rec}|Acc]);
import_vs([], _V, Acc) -> reverse(Acc).

import_vertex([<<X/float,Y/float,Z/float>>|T], _) ->
    import_vertex(T, {X,Y,Z});
import_vertex([_|T], Rec) ->
    import_vertex(T, Rec);
import_vertex([], Rec) -> Rec.

import_perm(Props) ->
    case proplists:get_value(state, Props) of
	undefined -> 0;
	locked -> 1;
	hidden -> 2;
	hidden_locked -> 3;
	{hidden,Mode,Set} -> {Mode,gb_sets:from_list(Set)};
	_Unknown -> 0
    end.

import_props([{selection,{Mode,Sel0}}|Ps], St) ->
    Sel = import_sel(Sel0, St),
    import_props(Ps, St#st{selmode=Mode,sel=Sel});
import_props([{saved_selection,{Mode,Sel0}}|Ps], St0) ->
    Sel = import_sel(Sel0, St0),
    St = new_sel_group(?__(1,"<Stored Selection>"), Mode, Sel, St0),
    import_props(Ps, St);
import_props([{{selection_group,Name},{Mode,Sel0}}|Ps], St0) ->
    Sel = import_sel(Sel0, St0),
    St = new_sel_group(Name, Mode, Sel, St0),
    import_props(Ps, St);
import_props([{lights,Lights}|Ps], St0) ->
    St = wings_light:import(Lights, St0),
    import_props(Ps, St);
import_props([{views,Views}|Ps], St0) ->
    St = wings_view:import_views(Views, St0),
    import_props(Ps, St);
import_props([{current_view,CurrentView}|Ps], #st{views={_,Views}}=St) ->
    import_props(Ps, St#st{views={CurrentView,Views}});
import_props([{palette,Palette}|Ps], St) ->
    import_props(Ps, St#st{pal=Palette});
import_props([{scene_prefs,ScenePrefs}|Ps], St) ->
    lists:foreach(fun({Key,Val}) ->
			  wings_pref:set_scene_value(Key, Val)
		  end,
		  ScenePrefs),
    import_props(Ps, St);
import_props([{plugin_states,Pst0}|Ps], #st{pst=Previous}=St0) ->
    St = try 
	     case gb_trees:keys(Previous) of
		 [] ->
		     Pst = gb_trees:from_orddict(lists:sort(Pst0)),
		     St0#st{pst=Pst};
		 _ when Pst0 =:= [] ->
		     St0;
		 PrevKeys ->
		     M=fun({Mod,Data},Acc) ->
			       case lists:member(Mod,PrevKeys) of
				   true ->
				       try
					   Pst = Mod:merge_st(Data,St0),
					   [{Mod,Pst}|lists:keydelete(Mod,1,Acc)]
				       catch _:_ -> Acc
				       end;
				   false ->
				       [{Mod,Data}|Acc]
			       end
		       end,
		     Pst1 = lists:foldl(M,gb_trees:to_list(Previous),Pst0),
		     Pst  = gb_trees:from_orddict(lists:sort(Pst1)),
		     St0#st{pst=Pst}
	     end
	 catch error:Reason -> 
		 io:format("Failed importing plugins state Not a gb_tree ~p ~n",
			   [Reason]),
		 St0
	 end,
    import_props(Ps,St);
import_props([_|Ps], St) ->
    import_props(Ps, St);
import_props([], St) -> St.

import_sel(Sel, #st{onext=IdBase}) ->
    [{IdBase+Id,gb_sets:from_list(Elems)} || {Id,Elems} <- Sel].

new_sel_group(Name, Mode, Sel, #st{ssels=Ssels0}=St) ->
    Key = {Mode,Name},
    case gb_trees:is_defined(Key, Ssels0) of
	true -> St;
	false ->
	    Ssels = gb_trees:insert(Key, Sel, Ssels0),
	    St#st{ssels=Ssels}
    end.

import_images(Dir,Props) ->
    Empty = gb_trees:empty(),
    case proplists:get_value(images, Props) of
	undefined -> Empty;
	Images -> import_images_1(Images, Dir, Empty)
    end.
	    
import_images_1([{Id0,Im}|T], Dir, Map) ->
    try 
	#e3d_image{name=Name} = E3D = import_image(Im,Dir),
	Id = wings_image:new(Name, E3D),
	import_images_1(T, Dir, gb_trees:insert(Id0, Id, Map))
    catch
	throw:{bad_image,Image} -> 
	    E3d = #e3d_image{name=Image,width=1,height=1,image= <<0,0,0>>},
	    ID = wings_image:new(Image, E3d),
	    import_images_1(T, Dir, gb_trees:insert(Id0, ID, Map))
    end;
import_images_1([], _, Map) -> Map.

import_image(Im,Dir) ->
    Name = proplists:get_value(name, Im, ?__(1,"unnamed image")),
    case proplists:get_value(filename, Im) of
	undefined ->
	    W = proplists:get_value(width, Im, 0),
	    H = proplists:get_value(height, Im, 0),
	    PP = proplists:get_value(samples_per_pixel, Im, 0),
	    Pixels = proplists:get_value(pixels, Im),
	    if
		W*H*PP =:= byte_size(Pixels) -> 
		    ok;
		true -> 
		    Str = io_lib:format(?__(2,"Bad image: ~ts\n"), [Name]),
		    wings_u:message(lists:flatten(Str)),
		    throw({bad_image,Name})
	    end,
	    MaskSize = proplists:get_value(mask_size, Im),
	    Type = case PP of
		       1 when MaskSize =:= 1 -> a8;
		       1 -> g8;
		       2 -> g8a8;
		       3 -> r8g8b8;
		       4 -> r8g8b8a8
		   end,
	    #e3d_image{name=Name,width=W,height=H,type=Type,order=lower_left,
		       alignment=1,bytes_pp=PP,image=Pixels};
	Filename ->
	    Ps = [{filename,Filename}, {opt_dir,Dir}],
	    case wings_image:image_read(Ps) of
		#e3d_image{}=E3D ->
		    E3D#e3d_image{name=Name};
		{error,_} ->
		    Str = io_lib:format(?__(2,"Bad image: ~ts\n"), [Name]),
		    wings_u:message(lists:flatten(Str)),
		    throw({bad_image,Name})
	    end
    end.

translate_map_images(Mats, ImMap) ->
    [translate_map_images_1(M, ImMap) || M <- Mats].

translate_map_images_1({Name,Props0}=Mat, ImMap) ->
    case proplists:get_value(maps, Props0, []) of
	[] -> Mat;
	Maps ->
	    Props = lists:keydelete(maps, 1, Props0),
	    {Name,[{maps,translate_map_images_2(Maps, Name, ImMap)}|Props]}
    end.

translate_map_images_2([{Type,Im0}|T], Mat, ImMap) when is_integer(Im0) ->
    case gb_trees:lookup(Im0, ImMap) of
	none ->
	    %% Something wrong here.
	    io:format( ?__(1,"Material ~p, ~p texture: reference to non-existing image ~p\n"),
		       [Mat,Type,Im0]),
	    translate_map_images_2(T, Mat, ImMap);
	{value,Im} ->
	    if Type == normal -> wings_image:is_normalmap(Im);
	       true -> ok
	    end,
	    [{Type,Im}|translate_map_images_2(T, Mat, ImMap)]
    end;
translate_map_images_2([H|T], Mat, ImMap) ->
    [H|translate_map_images_2(T, Mat, ImMap)];
translate_map_images_2([], _, _) -> [].

%%%
%%% Sharing of floating point numbers on import.
%%%

share_list(Wes) ->
    Tabs0 = [Tabs || {_,_,{_,_}=Tabs} <- Wes],
    Floats = share_floats(Tabs0, tuple_to_list(wings_color:white())),
    Tabs = share_list_1(Tabs0, Floats, gb_trees:empty(), []),
    share_list_2(Tabs, Wes, []).

share_list_1([{Vtab0,Etab0}|Ts], Floats, Tuples0, Acc) ->
    Vtab = share_vs(Vtab0, Floats, []),
    {Etab,Attr,Tuples} = share_es(Etab0, Floats, [], [], Tuples0),
    share_list_1(Ts, Floats, Tuples, [{Vtab,Etab,Attr}|Acc]);
share_list_1([], _, _, Ts) -> reverse(Ts).

share_list_2([{Vtab0,Etab0,Attr}|Ts],
	     [{NumHidden,#we{id=Id,mat=FaceMat}=We0,_}|Wes], Acc) ->
    Vtab = array:from_orddict(Vtab0),
    Etab = array:from_orddict(Etab0),
    We1 = wings_we:rebuild(We0#we{vp=Vtab,es=Etab,mat=default}),
    We2 = wings_facemat:assign(FaceMat, We1),

    %% Hide invisible faces and set holes.
    We3 = if
	      NumHidden =:= 0 -> We2;
	      true ->
		  Hidden = lists:seq(0, NumHidden-1),
		  Holes = ordsets:from_list([-F-1 || F <- We2#we.holes]),
		  wings_we:hide_faces(Hidden, We2#we{holes=Holes})
	  end,
    We4 = translate_old_holes(We3),
    We5 = validate_holes(We4),

    %% Very old Wings files can have invalid mirror faces for some reason.
    We6 = wings_we:validate_mirror(We5),

    %% Set attributes (if any) for all edges.
    We7 = foldl(fun({E,Lt,Rt}, W) ->
			wings_va:set_both_edge_attrs(E, Lt, Rt, W)
		end, We6, Attr),

    %% At last, hide the virtual mirror face.
    We = case We7 of
	     #we{mirror=none} ->
		 We7;
	     #we{mirror=MirrorFace} ->
		 %% Hide the virtual mirror face.
		 We8 = wings_we:hide_faces([MirrorFace], We7),
		 We8#we{mirror=-MirrorFace-1}
	 end,
    share_list_2(Ts, Wes, [{Id,We}|Acc]);
share_list_2([], [], Wes) -> sort(Wes).

share_floats([{Vtab,Etab}|T], Shared0) ->
    Shared1 = share_floats_1(Vtab, Shared0),
    Shared = share_floats_2(Etab, Shared1),
    share_floats(T, Shared);
share_floats([], Shared0) ->
    Shared1 = ordsets:from_list(Shared0),
    Shared = share_floats_4(Shared1, []),
    gb_trees:from_orddict(Shared).

share_floats_1([{_,{A,B,C}}|T], Shared) ->
    share_floats_1(T, [A,B,C|Shared]);
share_floats_1([], Shared) -> Shared.

share_floats_2([{_,{_,none}}|T], Shared) ->
    share_floats_2(T, Shared);
share_floats_2([{_,{_,#va{}=Va}}|T], Shared0) ->
    Shared1 = share_floats_3(Va#va.color_lt, Shared0),
    Shared2 = share_floats_3(Va#va.color_rt, Shared1),
    Shared3 = share_floats_3(Va#va.uv_lt, Shared2),
    Shared = share_floats_3(Va#va.uv_rt, Shared3),
    share_floats_2(T, Shared);
share_floats_2([], Shared) -> Shared.

share_floats_3({A,B}, [A,B|_]=Shared) -> Shared;
share_floats_3({A,B,C}, [A,B,C|_]=Shared) -> Shared;
share_floats_3({A,B}, Shared) -> [A,B|Shared];
share_floats_3({A,B,C}, Shared) -> [A,B,C|Shared];
share_floats_3(none, Shared) -> Shared.

share_floats_4([F|Fs], Acc) ->
    share_floats_4(Fs, [{F,F}|Acc]);
share_floats_4([], Acc) -> reverse(Acc).

share_vs([{V,{X0,Y0,Z0}}|Vs], Floats, Acc) ->
    X = gb_trees:get(X0, Floats),
    Y = gb_trees:get(Y0, Floats),
    Z = gb_trees:get(Z0, Floats),
    share_vs(Vs, Floats, [{V,{X,Y,Z}}|Acc]);
share_vs([], _, Acc) -> reverse(Acc).

share_es([{E,{Rec,none}}|Vs], Floats, Acc, AttrAcc, Shared) ->
    share_es(Vs, Floats, [{E,Rec}|Acc], AttrAcc, Shared);
share_es([{E,{Rec,Va0}}|Vs], Floats, Acc, AttrAcc0, Shared0) ->
    #va{color_lt=ColLt0,color_rt=ColRt0,
	uv_lt=UvLt0,uv_rt=UvRt0} = Va0,
    {ColLt,Shared1} = share_tuple(ColLt0, Floats, Shared0),
    {ColRt,Shared2} = share_tuple(ColRt0, Floats, Shared1),
    {UvLt,Shared3} = share_tuple(UvLt0, Floats, Shared2),
    {UvRt,Shared} = share_tuple(UvRt0, Floats, Shared3),
    LtAttr = wings_va:new_attr(ColLt, UvLt),
    RtAttr = wings_va:new_attr(ColRt, UvRt),
    AttrAcc = [{E,LtAttr,RtAttr}|AttrAcc0],
    share_es(Vs, Floats, [{E,Rec}|Acc], AttrAcc, Shared);
share_es([], _, Acc, AttrAcc, Shared) ->
    {reverse(Acc),AttrAcc,Shared}.

share_tuple({A0,B0}=Tuple0, Floats, Shared) ->
    case gb_trees:lookup(Tuple0, Shared) of
	none ->
	    A = gb_trees:get(A0, Floats),
	    B = gb_trees:get(B0, Floats),
	    Tuple = {A,B},
	    {Tuple,gb_trees:insert(Tuple, Tuple, Shared)};
	{value,Tuple} -> {Tuple,Shared}
    end;
share_tuple({A0,B0,C0}=Tuple0, Floats, Shared) ->
    case gb_trees:lookup(Tuple0, Shared) of
	none ->
	    A = gb_trees:get(A0, Floats),
	    B = gb_trees:get(B0, Floats),
	    C = gb_trees:get(C0, Floats),
	    Tuple = {A,B,C},
	    {Tuple,gb_trees:insert(Tuple, Tuple, Shared)};
	{value,Tuple} -> {Tuple,Shared}
    end;
share_tuple(none, _, Shared) -> {none,Shared}.

%%%
%%% Import of old materials format (up to and including wings-0.94.02).
%%%

translate_materials(Mats) ->
    [translate_material(M) || M <- Mats].
    
translate_material({Name,Props}=Mat) ->
    case proplists:is_defined(opengl, Props) of
	true -> Mat;
	false ->
	    Opac = proplists:get_value(opacity, Props),
	    {Name,translate_material(Props, Opac, [], [])}
    end.

translate_material([Mat|Mats], Opac, OpenGL, Maps) ->
    case Mat of
	{diffuse_map,Map} ->
	    translate_material(Mats, Opac, OpenGL, [{diffuse,Map}|Maps]);
	{diffuse,_}=Diff ->
	    translate_material(Mats, Opac, [trans(Diff, Opac)|OpenGL], Maps);
	{ambient,_}=Amb ->
	    translate_material(Mats, Opac, [trans(Amb, Opac)|OpenGL], Maps);
	{specular,_}=Spec ->
	    translate_material(Mats, Opac, [trans(Spec, Opac)|OpenGL], Maps);
	{shininess,Sh} ->
	    translate_material(Mats, Opac, [{shininess,1.0-Sh}|OpenGL], Maps);
	_ ->
	    translate_material(Mats, OpenGL, Opac, Maps)
    end;
translate_material([], _, OpenGL, Maps) ->
    [{opengl,OpenGL},{maps,Maps}].

trans({Key,{R,G,B}}, Opac) -> {Key,{R,G,B,Opac}}.

%%
%% Object modes were removed after the 1.1.7 release and
%% replaced with information about vertex colors in the
%% materials. At the same time the 'default' material was
%% changed to show vertex colors for the faces it was applied
%% to.
%%
%% Left alone, there would be two annoyances when loading
%% old models:
%%
%% 1. Vertex colors would not be shown.
%%
%% 2. Since the 'default' materials do not match, the 'default'
%%    material in the file will be renamed to 'default2' (or
%%    something similar) and there would be a new 'default'
%%    material.
%%
%% We will avoid both those annoyances by changing the 'default'
%% material in the file so that it is more likely to match
%% current 'default' material. We will only do this change if
%% the file contains an implicit object mode for some object,
%% i.e. was saved by 1.1.7 or earlier.
%%
translate_object_modes(Mats, Objects) ->
    OldFile = any(fun(Obj) ->
			  {object,_Name,_Winged,Props} = Obj,
			  keymember(mode, 1, Props)
		  end, Objects),
    case OldFile of
	false -> Mats;
	true -> [translate_object_mode(M) || M <- Mats]
    end.

translate_object_mode({default=Name,Props0}) ->
    OpenGL0 = proplists:get_value(opengl, Props0, []),
    OpenGL = [{vertex_colors,set}|OpenGL0],
    Props = [{opengl,OpenGL}|lists:keydelete(opengl, 1, Props0)],
    {Name,Props};
translate_object_mode(Mat) -> Mat.

%%
%% There used to be a '_hole_' material up to the 1.1.10 release.
%% The '_hole_' material was pre-defined and was specially handled
%% when exporting (faces having the material would not be exported)
%% and importing (missing faces would be created and assigned the
%% material).
%%
%% Translate faces with the '_hole_' material to the new type
%% of holes introduced after the 1.1.10 release.
%%
translate_old_holes(#we{holes=[]}=We) ->
    case wings_facemat:is_material_used('_hole_', We) of
	false -> We;
	true -> translate_old_holes_1(We)
    end;
translate_old_holes(We) -> We.

translate_old_holes_1(#we{fs=Ftab}=We0) ->
    MatFaces = wings_facemat:mat_faces(gb_trees:to_list(Ftab), We0),
    {_,Holes0} = keyfind('_hole_', 1, MatFaces),
    Holes = [F || {F,_} <- Holes0],
    We1 = wings_dissolve:faces(Holes, We0),
    NewHoleFaces = wings_we:new_items_as_ordset(face, We0, We1),
    We = wings_facemat:assign(default, NewHoleFaces, We1),
    HiddenNewHoleFaces = ordsets:from_list([-F-1 || F <- NewHoleFaces]),
    wings_we:hide_faces(NewHoleFaces, We#we{holes=HiddenNewHoleFaces}).

%% validate_holes(We0) -> We
%%  Remove any invalid entries in We#we.holes. Ideally, there should
%%  be no invalid entries, but because of bugs there could be.
%%
validate_holes(#we{fs=Ftab,holes=Holes0}=We) ->
    %% Only keep faces that exist and are invisible.
    Holes = [F || F <- Holes0, F < 0, gb_trees:is_defined(F, Ftab)],
    We#we{holes=Holes}.
    
%%%
%%% Save a Wings file (in version 2).
%%%

export(Name, St0) ->
    wings_pb:start( ?__(1,"saving")),
    wings_pb:update(0.01, ?__(2,"lights")),
    Lights = wings_light:export_bc(St0),
    Materials = case wings_pref:get_value(save_unused_materials) of
        true -> 
            #st{mat=Mat} = St0,
            gb_trees:to_list(Mat);
        false -> 
            wings_material:used_materials(St0)
    end,
    #st{shapes=Shs0,views={CurrentView,_}} = St = 
    remove_lights(St0),
    Sel0 = collect_sel(St),
    wings_pb:update(0.65, ?__(3,"renumbering")),
    Shs1 = [{Id,show_mirror_face(We)} ||
	       {Id,We} <- gb_trees:to_list(Shs0)],
    {Shs2,Sel} = renumber(Shs1, Sel0, 0, [], []),
    Shs = foldl(fun shape/2, [], Shs2),
    wings_pb:update(0.98, ?__(4,"objects")),
    Props0 = export_props(Sel),
    Props1 = case Lights of
		 [] -> Props0;
		 [_|_] -> [{lights,Lights}|Props0]
	     end,
    Props2 = case export_images() of
		[] -> Props1;
		Images -> [{images,Images}|Props1]
	     end,
    Props3 = case wings_view:export_views(St) of
		 [] -> Props2;
		 Views -> [{current_view,CurrentView},{views,Views}|Props2]
	     end,
    Props4 = case wings_palette:palette(St) of
		 [] -> Props3;
		 Palette -> [{palette, Palette}|Props3]
	     end,
    Props5 = export_pst(St#st.pst,Props4),
    Props  = [{scene_prefs,wings_pref:get_scene_value()}|Props5],
    Wings = {wings,2,{Shs,Materials,Props}},
    wings_pb:update(0.99, ?__(5,"compressing")),
    Bin = term_to_binary(Wings, [compressed]),
    wings_pb:update(1.0, ?__(6,"writing file")),
    wings_pb:done(write_file(Name, Bin)).

remove_lights(#st{sel=Sel0,shapes=Shs0}=St) ->
    Shs1 = foldl(fun(We, A) when ?IS_ANY_LIGHT(We) -> A;
		    (#we{id=Id}=We, A) -> [{Id,We}|A]
		 end, [], gb_trees:values(Shs0)),
    Shs = gb_trees:from_orddict(reverse(Shs1)),
    Sel = [S || {Id,_}=S <- Sel0, gb_trees:is_defined(Id, Shs)],
    St#st{sel=Sel,shapes=Shs}.

collect_sel(#st{selmode=Mode,sel=Sel0,ssels=Ssels}=St) ->
    Sel1 = [{Id,{Mode,gb_sets:to_list(Elems),selection}} ||
	       {Id,Elems} <- Sel0],
    Sel2 = collect_sel_groups(gb_trees:to_list(Ssels), St, Sel1),
    Sel3 = sofs:relation(Sel2, [{id,data}]),
    Sel = sofs:relation_to_family(Sel3),
    sofs:to_external(Sel).

collect_sel_groups([{{Mode,Name},Sel}|Gs], St, Acc0) ->
    Acc = [{Id,{Mode,gb_sets:to_list(Elems),{selection_group,Name}}} ||
	      {Id,Elems} <- wings_sel:valid_sel(Sel, Mode, St)] ++ Acc0,
    collect_sel_groups(Gs, St, Acc);
collect_sel_groups([], _, Acc) -> Acc.

show_mirror_face(#we{mirror=none}=We) -> We;
show_mirror_face(#we{mirror=Face}=We) ->
    %% The mirror face should not be hidden in a .wings file.
    %% (For compatibility with previous versions.)
    wings_we:show_faces([Face], We#we{mirror=-Face-1}).

renumber([{Id,We0}|Shs], [{Id,Root0}|Sel], NewId, WeAcc, RootAcc) ->
    Hidden = wings_we:num_hidden(We0),
    {We,Root} = wings_we:renumber(We0, 0, Root0),
    renumber(Shs, Sel, NewId+1, [{Hidden,We}|WeAcc],
	     [{NewId,Root}|RootAcc]);
renumber([{_,We0}|Shs], Sel, NewId, WeAcc, RootAcc) ->
    Hidden = wings_we:num_hidden(We0),
    We = wings_we:renumber(We0, 0),
    renumber(Shs, Sel, NewId+1, [{Hidden,We}|WeAcc], RootAcc);
renumber([], [], _NewId, WeAcc, RootAcc) ->
    {WeAcc,RootAcc}.

export_props(Sel0) ->
    Sel1 = sofs:family(Sel0, [{id,[{mode,list,key}]}]),
    Sel2 = sofs:family_to_relation(Sel1),
    Sel3 = sofs:projection(
	     {external,fun({Id,{Mode,Elems,Key}}) ->
			       {{Key,Mode},{Id,Elems}}
		       end}, Sel2),
    Sel = sofs:relation_to_family(Sel3),
    export_props_1(sofs:to_external(Sel), []).

export_props_1([{{What,Mode},Sel}|T], Acc) ->
    export_props_1(T, [{What,{Mode,Sel}}|Acc]);
export_props_1([], Acc) -> Acc.

export_pst(undefined, Props0) -> Props0;
export_pst(Pst0,Props0) ->
    try 
	Pst1 = gb_trees:to_list(Pst0),
	Pst = lists:filter(fun({Mod,_}) when is_atom(Mod) -> true;
			      (_) -> false end, Pst1),
	[{plugin_states,Pst}|Props0]
    catch error:Reason -> 
	    io:format("Failed exporting plugins state NOT a gb_tree ~p ~n",
		      [Reason]),
	    Props0
    end.

write_file(Name, Bin) ->
    Data = <<?WINGS_HEADER,(byte_size(Bin)):32,Bin/binary>>,
    case file:write_file(Name, Data) of
	ok -> ok;
	{error,Reason} -> {error,file:format_error(Reason)}
    end.

shape({Hidden,#we{name=Name,vp=Vs0,es=Es0,he=Htab,pst=Pst}=We}, Acc) ->
    Vs1 = foldl(fun export_vertex/2, [], array:sparse_to_list(Vs0)),
    Vs = reverse(Vs1),
    UvFaces = gb_sets:from_ordset(wings_we:uv_mapped_faces(We)),
    Es1 = array:sparse_foldl(fun(E, Rec, A) ->
				     export_edge(E, Rec, UvFaces, We, A)
			     end, [], Es0),
    Es = reverse(Es1),
    Fs1 = foldl(fun export_face/2, [], wings_facemat:all(We)),
    Fs = reverse(Fs1),
    He = gb_sets:to_list(Htab),
    Props0 = export_perm(We),
    Props1 = hidden_faces(Hidden, Props0),
    Props2 = mirror(We, Props1),
    Props3 = export_holes(We, Props2),
    Props  = export_pst(Pst, Props3),
    [{object,Name,{winged,Es,Fs,Vs,He},Props}|Acc].

mirror(#we{mirror=none}, Props) -> Props;
mirror(#we{mirror=Face}, Props) -> [{mirror_face,Face}|Props].

hidden_faces(0, Props) -> Props;
hidden_faces(N, Props) -> [{num_hidden_faces,N}|Props].

export_holes(#we{holes=[]}, Props) -> Props;
export_holes(#we{holes=Holes}, Props) -> [{holes,Holes}|Props].

export_perm(#we{perm=0}) -> [];
export_perm(#we{perm=1}) -> [{state,locked}];
export_perm(#we{perm=2}) -> [{state,hidden}];
export_perm(#we{perm=3}) -> [{state,hidden_locked}];
export_perm(#we{perm={Mode,Elems}}) ->
    [{state,{hidden,Mode,gb_sets:to_list(Elems)}}].

export_edge(E, Rec, UvFaces, We, Acc) ->
    #edge{vs=Va,ve=Vb,lf=Lf,rf=Rf,
	  ltpr=Ltpr,ltsu=Ltsu,rtpr=Rtpr,rtsu=Rtsu} = Rec,
    Data0 = [{edge,Va,Vb,Lf,Rf,Ltpr,Ltsu,Rtpr,Rtsu}],
    Data = edge_data(E, Rec, We, UvFaces, Data0),
    [Data|Acc].
    
edge_data(E, #edge{lf=Lf,rf=Rf}, We, UvFaces, Acc0) ->
    A = wings_va:edge_attrs(E, left, We),
    B = wings_va:edge_attrs(E, right, We),

    %% If there are both vertex colors and UV coordinates,
    %% we want them in the following order:
    %%   [{color_*,_},{uv_*,_}]
    %% On import in an old version of Wings, the UV coordinates
    %% will be used.
    Acc1 = edge_data_uv(left, Lf, wings_va:attr(uv, A), UvFaces, Acc0),
    Acc2 = edge_data_uv(right, Rf, wings_va:attr(uv, B), UvFaces, Acc1),
    Acc = edge_data_color(left, wings_va:attr(color, A), Acc2),
    edge_data_color(right, wings_va:attr(color, B), Acc).

edge_data_uv(Side, Face, {U,V}, UvFaces, Acc) ->
    case gb_sets:is_member(Face, UvFaces) of
	false -> Acc;
	true when Side =:= left  -> [{uv_lt,<<U/float,V/float>>}|Acc];
	true when Side =:= right -> [{uv_rt,<<U/float,V/float>>}|Acc]
    end;
edge_data_uv(_, _, _, _, Acc) -> Acc.

edge_data_color(left, {R,G,B}, Acc) ->
    [{color_lt,<<R:32/float,G:32/float,B:32/float>>}|Acc];
edge_data_color(right, {R,G,B}, Acc) ->
    [{color_rt,<<R:32/float,G:32/float,B:32/float>>}|Acc];
edge_data_color(_, _, Acc) -> Acc.

export_face({_,default}, Acc) -> [[]|Acc];
export_face({_,Mat}, Acc) -> [[{material,Mat}]|Acc].

export_vertex({X,Y,Z}, Acc) ->
    [[<<X/float,Y/float,Z/float>>]|Acc].

export_images() ->
    export_images_1(wings_image:images()).

export_images_1([{Id,Im}|T]) ->
    [{Id,export_image(Im)}|export_images_1(T)];
export_images_1([]) -> [].

export_image(#e3d_image{filename=none,type=Type0,order=Order}=Im0) ->
    Im = case {export_img_type(Type0),Order} of
	     {Type0=Type,lower_left} -> Im0;
	     {Type,_} -> e3d_image:convert(Im0, Type, 1, lower_left)
	 end,
    #e3d_image{width=W,height=H,bytes_pp=PP,image=Pixels,name=Name} = Im,
    MaskSize = mask_size(Type),
    [{name,Name},{width,W},{height,H},{samples_per_pixel,PP},
     {mask_size,MaskSize},{pixels,Pixels}];
export_image(#e3d_image{name=Name,filename=Filename}=Im) ->
    case filelib:is_file(Filename) of
	false ->
	    export_image(Im#e3d_image{filename=none});
	true ->
	    [{name,Name},{filename,Filename}]
    end.

export_img_type(b8g8r8) -> r8g8b8;
export_img_type(b8g8r8a8) -> r8g8b8a8;
export_img_type(Type) -> Type.

mask_size(r8g8b8a8) -> 1;
mask_size(a8) -> 1;
mask_size(_) -> 0.
