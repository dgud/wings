%%
%%  wings_shape.erl --
%%
%%     Utilities for shape records.
%%
%%  Copyright (c) 2001-2011 Bjorn Gustavsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%

-module(wings_shape).
-export([new/3,insert/3,replace/3,merge_st/2,merge_we/1]).
-export([all_selectable/1]).
-export([show_all/1, show_all_in_folder/2,
	 unlock_all/1, unlock_all_in_folder/2,
	 lock_object/2, lock_others/2, lock_others_in_folder/3,
	 unlock_object/2,
	 show_object/2,
	 hide_object/2, hide_others/2, hide_others_in_folder/3,
	 permissions/3]).

-export([create_folder_system/1,recreate_folder_system/1,update_folders/1,
	 create_folder/2, rename_folder/3, delete_folder/2, empty_folder/2,
	 move_to_folder/2, move_to_folder/3, folder_status/2, folder_info/2
	]).

-export([draw_bitmap_16/3,cube_bitmap/0,selcube_bitmap/0,
	 vertex_sel_cube_bitmap/0,face_sel_cube_bitmap/0,edge_sel_cube_bitmap/0,
	 light_bitmap_0/0,light_bitmap_1/0]).
-export([locked_bitmap/0, unlocked_bitmap/0,
	 eye_bitmap/0, eye_closed_bitmap/0, eye_bg_bitmap/0,
	 empty_folder_bitmap/0, open_folder_bitmap/0, folder_fill_bitmap/0, closed_folder_bitmap/0
	]).

-define(FOLDERS,?MODULE).
-define(NO_FLD, no_folder).

-include("wings.hrl").
-import(lists, [map/2,foldl/3,reverse/1,reverse/2,
		keyfind/3,sort/1]).

%%%
%%% Exported functions.
%%%

%% new(Name, We, St0) -> St.
%%  Create a new object having the given name,
%%  converting all unknown materials to default.
new(Name, #we{pst=WePst}=We0, #st{shapes=Shapes0,onext=Oid,mat=Mat,pst=StPst}=St) ->
    UsedMat = wings_facemat:used_materials(We0),
    We =
	case lists:filter(
	       fun (M) -> not gb_trees:is_defined(M, Mat) end, 
	       UsedMat) of
	    [] -> We0;
	    XMat ->
		FMs = lists:filter(fun ({_,M}) -> lists:member(M, XMat) end,
				   wings_facemat:all(We0)),
		wings_facemat:assign(default, [F||{F,_}<-FMs], We0)
	end,
    Shapes = gb_trees:insert(Oid, We#we{name=Name,id=Oid}, Shapes0),
    {DefaultFolder0,FldList} = gb_trees:get(?FOLDERS, StPst),

	DefaultFolder=case gb_trees:is_empty(WePst) of  % check needed for compatibilty (old wings files hasn't this field)
	false ->
		WeFolder0 = gb_trees:get(wings_shape, WePst),  % checking for we's folder settings
		case lists:keymember(WeFolder0,1,FldList) of  % validating the folder
		true ->
			WeFolder0;
		false ->
			DefaultFolder0
		end;
	true ->
		DefaultFolder0
    end,
    add_to_folder(DefaultFolder, Oid, St#st{shapes=Shapes,onext=Oid+1}).

%% new(We, Suffix, St0) -> St.
%%  Suffix = cut | clone | copy | extract | sep
%%
%%  Create a new object based on an old object. The name
%%  will be created from the old name (with digits and known
%%  suffixes stripped) with the given Suffix and a number
%%  appended.
insert(#we{id=Id,name=OldName}=We0, Suffix, #st{shapes=Shapes0,onext=Oid}=St) ->
    Name = new_name(OldName, Suffix, Oid),
    We = We0#we{id=Oid,name=Name},
    Shapes = gb_trees:insert(Oid, We, Shapes0),
    FolderName = folder_name(Id, St),
    add_to_folder(FolderName, Oid, St#st{shapes=Shapes,onext=Oid+1}).

replace(Id, #we{id=OldId}=We0, #st{shapes=Shapes0}=St0) ->
    We = We0#we{id=Id},
    Shapes = gb_trees:update(Id, We, Shapes0),
    FolderName = folder_name(OldId, St0),
    St = update_folders(St0#st{shapes=Shapes}),
    add_to_folder(FolderName, Id, St).

permissions(We, Visible, Locked) ->
    P0 = case Visible of
	     true -> 0;
	     false -> 2
	 end,
    P = case Locked of
	    true -> P0 bor 1;
	    false -> P0
	end,
    We#we{perm=P}.

show_all_in_folder(Ids, #st{shapes=Shs0,sel=Sel0}=St) ->
    Shs1 = gb_trees:values(Shs0),
    Shs2 = map(fun(#we{id=Id}=We) ->
        case lists:member(Id, Ids) of
            true -> {Id,show_we(We)};
            false -> {Id,We}
        end
    end, Shs1),
    Shs = gb_trees:from_orddict(Shs2),
    Sel = lists:usort(show_all_sel_in_folder(Ids, Shs1, St, Sel0)),
    St#st{shapes=Shs,sel=Sel}.

show_all(#st{shapes=Shs0,sel=Sel0}=St) ->
    Shs1 = gb_trees:values(Shs0),
    Shs2 = [{Id,show_we(We)} || #we{id=Id}=We <- Shs1],
    Shs = gb_trees:from_orddict(Shs2),
    Sel = sort(show_all_sel(Shs1, St, Sel0)),
    St#st{shapes=Shs,sel=Sel}.

unlock_all_in_folder(Ids, #st{shapes=Shs0}=St) ->
    Shs1 = gb_trees:values(Shs0),
    Shs2 = map(fun(#we{id=Id}=We) ->
        case lists:member(Id, Ids) of
            true -> {Id,maybe_unlock(We)};
            false -> {Id,We}
        end
    end, Shs1),
    Shs = gb_trees:from_orddict(Shs2),
    St#st{shapes=Shs}.

unlock_all(#st{shapes=Shs0}=St) ->
    Shs1 = gb_trees:values(Shs0),
    Shs2 = [{Id,maybe_unlock(We)} || #we{id=Id}=We <- Shs1],
    Shs = gb_trees:from_orddict(Shs2),
    St#st{shapes=Shs}.

%% all_selectable(St) -> GbSet
%%  Return a GbSet containing IDs for all selectable objects (i.e. not locked).
all_selectable(#st{shapes=Shs}) ->
    all_selectable_1(gb_trees:to_list(Shs), []).

all_selectable_1([{Id,#we{perm=P}}|T], Acc) when ?IS_SELECTABLE(P) ->
    all_selectable_1(T, [Id|Acc]);
all_selectable_1([_|T], Acc) ->
    all_selectable_1(T, Acc);
all_selectable_1([], Acc) -> gb_sets:from_ordset(reverse(Acc)).

%%%
%%% Local functions follow.
%%%

new_name(OldName, Suffix0, Id) ->
    Suffix = suffix(Suffix0),
    Base = base(reverse(OldName)),
    reverse(Base, "_" ++ Suffix ++ integer_to_list(Id)).

%% Note: Filename suffixes are intentionally not translated.
%% If we are to translate them in the future, base/1 below
%% must be updated to strip suffixes (both for the current language
%% and for English).

suffix(cut) -> "cut";
suffix(clone) -> "clone";
suffix(copy) -> "copy";
suffix(extract) -> "extract";
suffix(mirror) -> "mirror";
suffix(sep) -> "sep".

%% base_1(ReversedName) -> ReversedBaseName
%%  Given an object name, strip digits and known suffixes to
%%  create a base name. Returns the unchanged name if
%%  no known suffix could be stripped.

base(OldName) ->
    case base_1(OldName) of
	error -> OldName;
	Base -> Base
    end.

base_1([H|T]) when $0 =< H, H =< $9 -> base_1(T);
base_1("tuc_"++Base) -> Base;			%"_cut"
base_1("enolc_"++Base) -> Base;			%"_clone"
base_1("ypoc_"++Base) -> Base;			%"_copy"
base_1("tcartxe_"++Base) -> Base;		%"_extract"
base_1("rorrim_"++Base) -> Base;		%"_mirror"
base_1("pes_"++Base) -> Base;			%"_sep"
base_1(_Base) -> error.

merge_st({_,_}=Data, _) ->
    Data;
merge_st(_,_) ->
    {?NO_FLD,orddict:new()}.

merge_we(Wes) ->
    Flds = foldl(fun(#we{pst=Pst}, Acc) ->
        Folder = gb_trees:get(?FOLDERS, Pst),
        [Folder|Acc]
    end, [], Wes),
    case lists:usort(Flds) of
        [Fld] -> Fld;
        _ -> ?NO_FLD
    end.


%%%
%%% Utilities.
%%%

hide_object(Id, #st{shapes=Shs0}=St) ->
    We0 = gb_trees:get(Id, Shs0),
    We = hide_we(We0, St),
    Shs = gb_trees:update(Id, We, Shs0),
    wings_sel:deselect_object(Id, St#st{shapes=Shs}).

hide_we(#we{id=Id,perm=Perm0}=We, St) ->
    Perm = case get_sel(Id, St) of
	       [] -> Perm0 bor 2;
	       Other -> Other
	   end,
    We#we{perm=Perm}.

show_object(Id, #st{shapes=Shs0}=St) ->
    We0 = gb_trees:get(Id, Shs0),
    Sel = update_sel(We0, St),
    We = show_we(We0),
    Shs = gb_trees:update(Id, We, Shs0),
    St#st{sel=Sel,shapes=Shs}.

show_we(#we{perm=Perm0}=We) ->
    Perm = case Perm0 of
	       3 -> 1;
	       1 -> 1;
	       _ -> 0
	   end,
    We#we{perm=Perm}.

lock_object(Id, St0) ->
    St = wings_sel:deselect_object(Id, St0),
    update_permission(1, Id, St).

unlock_object(Id, St) ->
    update_permission(0, Id, St).

update_permission(Perm, Id, #st{shapes=Shs0}=St) ->
    We = gb_trees:get(Id, Shs0),
    Shs = gb_trees:update(Id, We#we{perm=Perm}, Shs0),
    Sel = update_sel(We, St),
    St#st{sel=Sel,shapes=Shs}.

hide_others_in_folder(ThisId, Ids, #st{shapes=Shs0,sel=Sel0}=St) ->
    Shs1 = map(fun(#we{id=Id}=We) when Id =:= ThisId -> {Id,We};
		  (#we{id=Id}=We) ->
		      case lists:member(Id, Ids) of
		          true -> {Id,hide_we(We, St)};
		          false -> {Id,We}
		      end
	       end, gb_trees:values(Shs0)),
    Shs = gb_trees:from_orddict(Shs1),
    Sel = foldl(fun({Id,_}=S, A) ->
		      case lists:member(Id, Ids) of
		          true when Id =:= ThisId -> [S|A];
		          true -> A;
		          false -> [S|A]
		      end
	       end, [], Sel0),
    St#st{shapes=Shs,sel=lists:usort(Sel)}.

hide_others(ThisId, #st{shapes=Shs0,sel=Sel0}=St) ->
    Shs1 = map(fun(#we{id=Id}=We) when ThisId =:= Id -> {Id,We};
		  (#we{id=Id}=We) ->
		       {Id,hide_we(We, St)}
	       end, gb_trees:values(Shs0)),
    Shs = gb_trees:from_orddict(Shs1),
    Sel = [This || {Id,_}=This <- Sel0, Id =:= ThisId],
    St#st{shapes=Shs,sel=Sel}.

show_all_sel([#we{id=Id,perm={Mode,Set}}|T], #st{selmode=Mode}=St, Acc) ->
    show_all_sel(T, St, [{Id,Set}|Acc]);
show_all_sel([#we{id=Id,perm={SMode,Set0}}|T], #st{selmode=Mode}=St, Acc) ->
    StTemp = St#st{selmode=SMode,sel=[{Id,Set0}]},
    #st{sel=[{Id,Set}]} = wings_sel_conv:mode(Mode, StTemp),
    show_all_sel(T, St, [{Id,Set}|Acc]);
show_all_sel([_|T], St, Acc) ->
    show_all_sel(T, St, Acc);
show_all_sel([], _St, Acc) -> Acc.

show_all_sel_in_folder(Ids, [#we{id=Id,perm={Mode,Set}}|T], #st{selmode=Mode}=St, Acc) ->
    case lists:member(Id, Ids) of
        true -> show_all_sel_in_folder(Ids, T, St, [{Id,Set}|Acc]);
        false -> show_all_sel_in_folder(Ids, T, St, Acc)
    end;
show_all_sel_in_folder(Ids, [#we{id=Id,perm={SMode,Set0}}|T], #st{selmode=Mode}=St, Acc) ->
    case lists:member(Id, Ids) of
        true ->
            StTemp = St#st{selmode=SMode,sel=[{Id,Set0}]},
            #st{sel=[{Id,Set}]} = wings_sel_conv:mode(Mode, StTemp),
            show_all_sel_in_folder(Ids, T, St, [{Id,Set}|Acc]);
        false -> show_all_sel_in_folder(Ids, T, St, Acc)
    end;
show_all_sel_in_folder(Ids, [_|T], St, Acc) ->
    show_all_sel_in_folder(Ids, T, St, Acc);
show_all_sel_in_folder(_, [], _, Acc) -> Acc.

lock_others_in_folder(ThisId, Ids, #st{shapes=Shs0,sel=Sel0}=St) ->
    Shs1 = map(fun(#we{id=Id}=We) when ThisId =:= Id ->
		       {Id,We};
		  (#we{id=Id,perm=P}=We) when ?IS_VISIBLE(P) ->
		      case lists:member(Id, Ids) of
		          true -> {Id,We#we{perm=1}};
		          false -> {Id,We}
		      end;
		  (#we{id=Id}=We) ->
		       {Id,We}
	       end, gb_trees:values(Shs0)),
    Shs = gb_trees:from_orddict(Shs1),
    Sel = foldl(fun({Id,_}=S, A) ->
		      case lists:member(Id, Ids) of
		          true when Id =:= ThisId -> [S|A];
		          true -> A;
		          false -> [S|A]
		      end
	       end, [], Sel0),
    St#st{shapes=Shs,sel=Sel}.

lock_others(ThisId, #st{shapes=Shs0,sel=Sel0}=St) ->
    Shs1 = map(fun(#we{id=Id}=We) when ThisId =:= Id ->
		       {Id,We};
		  (#we{id=Id,perm=P}=We) when ?IS_VISIBLE(P) ->
		       {Id,We#we{perm=1}};
		  (#we{id=Id}=We) ->
		       {Id,We}
	       end, gb_trees:values(Shs0)),
    Shs = gb_trees:from_orddict(Shs1),
    Sel = [This || {Id,_}=This <- Sel0, Id =:= ThisId],
    St#st{shapes=Shs,sel=Sel}.

maybe_unlock(#we{perm=P}=We) when ?IS_VISIBLE(P) -> We#we{perm=0};
maybe_unlock(We) -> We.
    
get_sel(Id, #st{selmode=Mode,sel=Sel}) ->
    case keyfind(Id, 1, Sel) of
	false -> [];
	{Id,Set} -> {Mode,Set}
    end.

update_sel(#we{id=Id,perm={Mode,Set}}, #st{selmode=Mode,sel=Sel}) ->
    sort([{Id,Set}|Sel]);
update_sel(#we{id=Id,perm={SMode,Elems0}}, #st{selmode=Mode,sel=Sel}=St) ->
    StTemp = St#st{selmode=SMode,sel=[{Id,Elems0}]},
    #st{sel=[{Id,Elems}]} = wings_sel_conv:mode(Mode, StTemp),
    sort([{Id,Elems}|Sel]);
update_sel(_, #st{sel=Sel}) -> Sel.

%%%% 

%%%
%%% Folders
%%%

%% [{Folder,{Status[open|closed],Ids gb_set()} | _]

create_folder_system(#st{pst=Pst0}=St) ->
    case gb_trees:lookup(?FOLDERS, Pst0) of
        none ->
            Fld =  orddict:new(),
            Pst = gb_trees:insert(?FOLDERS, {?NO_FLD,Fld}, Pst0),
            St#st{pst=Pst};
        {value,_} -> St
    end.

recreate_folder_system(#st{shapes=Shs0,pst=StPst0}=St) ->
    {DefaultFld,Fld0} = gb_trees:get(?FOLDERS, StPst0),
    EmptyFlds = [{F,{Status,gb_sets:new()}} || {F,{Status,_}} <- Fld0],
    Wes = gb_trees:values(Shs0),
    {Shapes,Fld} = lists:mapfoldl(fun(#we{id=Id,pst=Pst0}=We, Fld1)->
        case gb_trees:lookup(?FOLDERS, Pst0) of
            {value,Folder} ->
                %% There are folders!
                case orddict:find(Folder, Fld1) of
                    {_,{Status,Ids0}} ->
                        %% Folder exist already,so add Id to it
                        Ids = gb_sets:add_element(Id, Ids0),
                        Fld2 = orddict:store(Folder, {Status,Ids}, Fld1),
                        {{Id,We},Fld2};
                    error ->
                        %% Folder doesn't exist in Fld, so add Folder with Id
                        Ids = gb_sets:singleton(Id),
                        Fld2 = orddict:store(Folder, {open,Ids}, Fld1),
                        {{Id,We},Fld2}
                end;
            none when Fld1 =:= []->
                %% no folder defined for this #we, and no folders anyway
                Ids = gb_sets:singleton(Id),
                Fld2 = orddict:store(?NO_FLD, {open,Ids}, Fld1),
                Pst = gb_trees:insert(?FOLDERS, ?NO_FLD, Pst0),
                {{Id,We#we{pst=Pst}},Fld2};
            none ->
                %% no folder specified, so place Id in DefaultFolder
                {Status,Ids0} = orddict:fetch(DefaultFld,Fld1),
                Ids = gb_sets:add(Id, Ids0),
                Fld2 = orddict:store(DefaultFld, {Status,Ids}, Fld1),
                Pst = gb_trees:insert(?FOLDERS, DefaultFld, Pst0),
                {{Id,We#we{pst=Pst}},Fld2}
        end
    end, lists:sort(EmptyFlds), Wes),
    Shs = gb_trees:from_orddict(Shapes),
    StPst = gb_trees:enter(?FOLDERS, {DefaultFld,Fld}, StPst0),
    St#st{shapes=Shs,pst=StPst}.

create_folder(Folder, #st{pst=Pst0}=St) ->
    {_,Fld0} = gb_trees:get(?FOLDERS, Pst0),
    Fld = case orddict:is_key(Folder, Fld0) of
        true -> wings_u:error_msg(?__(1,"A folder by that name already exists"));
        false -> orddict:store(Folder, {open,gb_sets:new()}, Fld0)
    end,
    Pst = gb_trees:update(?FOLDERS, {Folder,Fld}, Pst0),
    St#st{pst=Pst}.

rename_folder(OldName, NewName, St0) ->
    #st{pst=Pst0}=St1 = create_folder(NewName, St0),
    {_,Fld0} = gb_trees:get(?FOLDERS, Pst0),
    {_,Ids} = orddict:fetch(OldName, Fld0),
    Fld1 = orddict:erase(OldName, Fld0),
    Fld = orddict:store(NewName, {open,Ids}, Fld1),
    Pst = gb_trees:update(?FOLDERS, {NewName,Fld}, Pst0),
    St = register_ids_to_folder(NewName, Ids, St1),
    St#st{pst=Pst}.

add_to_folder(Folder, Id, #st{pst=Pst0}=St0) ->
    {_,Fld0} = gb_trees:get(?FOLDERS, Pst0),
    FData = case orddict:find(Folder, Fld0) of
        {_,{Status,Ids}} ->
            {Status,gb_sets:add(Id, Ids)};
        error -> {open,gb_sets:singleton(Id)}
    end,
    Fld = orddict:store(Folder, FData, Fld0),
    Pst = gb_trees:update(?FOLDERS, {Folder,Fld}, Pst0),
    St = register_ids_to_folder(Folder, Id, St0),
    St#st{pst=Pst}.

delete_folder(Folder, #st{shapes=Shs0,pst=Pst0}=St) ->
%% Delete folder and its contents
    {_,Fld0} = gb_trees:get(?FOLDERS, Pst0),
    {_,Ids} = orddict:fetch(Folder, Fld0),
    Shapes = foldl(fun(Id, Shs) ->
        gb_trees:delete(Id, Shs)
    end, Shs0, gb_sets:to_list(Ids)),
    Fld = orddict:erase(Folder, Fld0),
    DefaultFld = case Fld of
        [] -> ?NO_FLD;
        [{F,_}|_] -> F
    end,
    Pst = gb_trees:update(?FOLDERS, {DefaultFld,Fld}, Pst0),
    wings_sel:valid_sel(St#st{shapes=Shapes,pst=Pst}).

move_to_folder(Folder, #st{sel=Sel}=St) ->
    Ids = orddict:fetch_keys(Sel),
    move_to_folder(Folder, Ids, St).

move_to_folder(Folder, Ids, #st{shapes=Shs0,pst=Pst0}=St) ->
    {_,Fld0} = gb_trees:get(?FOLDERS, Pst0),
    {Shapes,Fld} = lists:foldl(fun(Id, {Shs1,Fld1}) ->
        #we{pst=WePst0}=We = gb_trees:get(Id, Shs1),
        OldFolder = gb_trees:get(?FOLDERS, WePst0),
        {StatusA,IdsA0} = orddict:fetch(OldFolder, Fld1),
        IdsA = gb_sets:delete(Id, IdsA0),
        Fld2 = orddict:store(OldFolder, {StatusA,IdsA}, Fld1),
        {StatusB,IdsB0} = orddict:fetch(Folder, Fld2),
        IdsB = gb_sets:add(Id, IdsB0),
        Fld3 = orddict:store(Folder, {StatusB,IdsB}, Fld2),
        WePst = gb_trees:update(?FOLDERS, Folder, WePst0),
        Shs = gb_trees:update(Id, We#we{pst=WePst}, Shs1),
        {Shs,Fld3}
    end, {Shs0,Fld0}, Ids),
    Pst = gb_trees:enter(?FOLDERS, {Folder,Fld}, Pst0),
    St#st{shapes=Shapes,pst=Pst}.

empty_folder(Folder, #st{pst=Pst}=St) ->
    {_,Fld} = gb_trees:get(?FOLDERS, Pst),
    {_,Ids} = orddict:fetch(Folder, Fld),
    move_to_folder(?NO_FLD, gb_sets:to_list(Ids), St).

register_ids_to_folder(Folder, Id, St) when is_integer(Id) ->
    register_ids_to_folder(Folder, [Id], St);
register_ids_to_folder(Folder, Ids, St0) when is_list(Ids) ->
    foldl(fun(Id, #st{shapes=Shs0}=St) ->
        #we{pst=Pst0}=We = gb_trees:get(Id, Shs0),
        Pst = gb_trees:enter(?FOLDERS, Folder, Pst0),
        Shs = gb_trees:update(Id, We#we{pst=Pst}, Shs0),
        St#st{shapes=Shs}
    end, St0, Ids);
register_ids_to_folder(Folder, Ids0, St) ->
    Ids = gb_sets:to_list(Ids0),
    register_ids_to_folder(Folder, Ids, St).

update_folders(#st{shapes=Shs,pst=Pst0}=St) ->
%% Assume that only when objects are deleted, will the folder system be corrupted.
    IdsA = gb_sets:from_list(gb_trees:keys(Shs)),
    {DefaultFld,Fld0} = gb_trees:get(?FOLDERS, Pst0),
    Fld = foldl(fun({Folder,{Status,Ids0}}, Fld1) ->
        RIds = gb_sets:subtract(Ids0, IdsA),
        case gb_sets:is_empty(RIds) of
            true -> Fld1;
            false ->
                Ids = gb_sets:subtract(Ids0, RIds),
                orddict:store(Folder, {Status,Ids}, Fld1)
        end
    end, Fld0, Fld0),
    Pst = gb_trees:enter(?FOLDERS, {DefaultFld,Fld}, Pst0),
    St#st{shapes=Shs,pst=Pst}.

folder_name(Id, #st{shapes=Shs}) ->
    #we{pst=Pst} = gb_trees:get(Id, Shs),
    gb_trees:get(?FOLDERS, Pst).

folder_status(Folder, #st{pst=Pst0}) ->
    {_,Fld} = gb_trees:get(?FOLDERS, Pst0),
    {Status,Ids} = orddict:fetch(Folder, Fld),
    case gb_sets:is_empty(Ids) of
        true -> empty;
        false -> Status
    end.

folder_info(Folder, #st{pst=Pst}) ->
    {DefaultFld,Fld} = gb_trees:get(?FOLDERS, Pst),
    {_,Ids} = orddict:fetch(Folder, Fld),
    Objects = io_lib:format("(~p)", [gb_sets:size(Ids)]),
    case DefaultFld =:= Folder of
        true -> Objects++" "++[crossmark];
        false -> Objects
    end.

%%%% Bitmaps for Outliner and Geometry Graph

draw_bitmap_16(X, Y, Bitmap) ->
    gl:rasterPos2i(X, Y),
    gl:bitmap(16, 16, 0.0, 0.0, 16.0, 0.0, Bitmap).

locked_bitmap() ->
    <<
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0011111111111000:16,
    2#0011111111111000:16,
    2#0011111011111000:16,
    2#0011111011111000:16,
    2#0011111011111000:16,
    2#0011111111111000:16,
    2#0011111111111000:16,
    2#0000110001100000:16,
    2#0000110001100000:16,
    2#0000110001100000:16,
    2#0000011111000000:16,
    2#0000001110000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16>>.

unlocked_bitmap() ->
    <<
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0111111111110000:16,
    2#0111111111110000:16,
    2#0111110111110000:16,
    2#0111110111110000:16,
    2#0111110111110000:16,
    2#0111111111110000:16,
    2#0111111111110000:16,
    2#0000000011000110:16,
    2#0000000011000110:16,
    2#0000000011000110:16,
    2#0000000001111100:16,
    2#0000000000111000:16,
    2#0000000000000000:16,
    2#0000000000000000:16>>.

eye_bitmap() ->
    <<
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000011111000000:16,
    2#0001110000110000:16,
    2#0011001111001000:16,
    2#0010011111100100:16,
    2#0001011110101000:16,
    2#0010111101110100:16,
    2#0001001111001000:16,
    2#0000110000110000:16,
    2#0000001111000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16>>.

eye_closed_bitmap() ->
    <<
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000011111000000:16,
    2#0001110000110000:16,
    2#0011000000001000:16,
    2#0010000000000100:16,
    2#0000000000000000:16,
    2#0010000000000100:16,
    2#0001000000001000:16,
    2#0000110000110000:16,
    2#0000001111000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16>>.

eye_bg_bitmap() ->
    <<
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000011111000000:16,
    2#0001111111110000:16,
    2#0011111111111000:16,
    2#0111111111111100:16,
    2#0111111111111110:16,
    2#0011111111111100:16,
    2#0111111111111110:16,
    2#0011111111111100:16,
    2#0001111111111000:16,
    2#0000111111110000:16,
    2#0000001111000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16>>.

cube_bitmap() ->
    <<
    2#0000000000000000:16,
    2#0000000111000000:16,
    2#0000011010110000:16,
    2#0001100010001100:16,
    2#0010000010000010:16,
    2#0010000010000010:16,
    2#0010000010000010:16,
    2#0010000010000010:16,
    2#0010000010000010:16,
    2#0010001101100010:16,
    2#0010110000011010:16,
    2#0011000000000110:16,
    2#0000111000111000:16,
    2#0000000111000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16>>.

edge_sel_cube_bitmap() ->
    <<
    2#0000000000000000:16,
    2#0000000011000000:16,
    2#0000000010110000:16,
    2#0000000010001100:16,
    2#0000000010000010:16,
    2#0000000010000010:16,
    2#0000000010000010:16,
    2#0000000010000010:16,
    2#0000000010000010:16,
    2#0000000001100010:16,
    2#0000000000011010:16,
    2#0000000000000110:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16>>.

selcube_bitmap() ->
    <<
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000101000000:16,
    2#0000011101110000:16,
    2#0001111101111100:16,
    2#0001111101111100:16,
    2#0001111101111100:16,
    2#0001111101111100:16,
    2#0001111101111100:16,
    2#0001110010011100:16,
    2#0001001111100100:16,
    2#0000111111111000:16,
    2#0000000111000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16>>.

face_sel_cube_bitmap() ->
    <<
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000001000000:16,
    2#0000000001110000:16,
    2#0000000001111100:16,
    2#0000000001111100:16,
    2#0000000001111100:16,
    2#0000000001111100:16,
    2#0000000001111100:16,
    2#0000000000011100:16,
    2#0000000000000100:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16>>.

vertex_sel_cube_bitmap() ->
    <<
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0011000000000000:16,
    2#0111100000000000:16,
    2#0011000000000000:16,
    2#0000000000000000:16,
    2#0000000111000000:16,
    2#0000000111000000:16,
    2#0000000111000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16>>.

light_bitmap_0() ->
    <<
    2#0000001110000000:16,
    2#0000011011000000:16,
    2#0000010101000000:16,
    2#0000011011000000:16,
    2#0000011111000000:16,
    2#0000110101100000:16,
    2#0001000100010000:16,
    2#0001000100010000:16,
    2#0010001010001000:16,
    2#0010000000001000:16,
    2#0010000000001000:16,
    2#0001000000010000:16,
    2#0001000000010000:16,
    2#0000110001100000:16,
    2#0000001110000000:16,
    2#0000000000000000:16>>.

light_bitmap_1() ->
    <<
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000001010000000:16,
    2#0000111011100000:16,
    2#0000111011100000:16,
    2#0001110101110000:16,
    2#0001111111110000:16,
    2#0001111111110000:16,
    2#0000111111100000:16,
    2#0000111111100000:16,
    2#0000001110000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16>>.

empty_folder_bitmap() ->
    <<
    2#0000000000000000:16,
    2#0011111111111100:16,
    2#0100000000000010:16,
    2#0100000000000010:16,
    2#0100000000000010:16,
    2#0100000000000010:16,
    2#0100000000000010:16,
    2#0100000000000010:16,
    2#0100000000000010:16,
    2#0100000000000010:16,
    2#0111111111111100:16,
    2#0100000010000000:16,
    2#0100000100000000:16,
    2#0011111000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16>>.

open_folder_bitmap() ->
    <<
    2#0000000000000000:16,
    2#0011111111111100:16,
    2#0100000000000010:16,
    2#0100000110000010:16,
    2#0100000110000010:16,
    2#0100011111100010:16,
    2#0100011111100010:16,
    2#0100000110000010:16,
    2#0100000110000010:16,
    2#0100000000000010:16,
    2#0111111111111100:16,
    2#0100000010000000:16,
    2#0100000100000000:16,
    2#0011111000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16>>.

closed_folder_bitmap() ->
    <<
    2#0000000000000000:16,
    2#0011111111111100:16,
    2#0100000000000010:16,
    2#0100000000000010:16,
    2#0100000000000010:16,
    2#0100011111100010:16,
    2#0100011111100010:16,
    2#0100000000000010:16,
    2#0100000000000010:16,
    2#0100000000000010:16,
    2#0111111111111100:16,
    2#0100000010000000:16,
    2#0100000100000000:16,
    2#0011111000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16>>.

folder_fill_bitmap() ->
    <<
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0011111111111100:16,
    2#0011111111111100:16,
    2#0011111111111100:16,
    2#0011111111111100:16,
    2#0011111111111100:16,
    2#0011111111111100:16,
    2#0011111111111100:16,
    2#0011111111111100:16,
    2#0000000000000000:16,
    2#0011111100000000:16,
    2#0011111000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16,
    2#0000000000000000:16>>.
